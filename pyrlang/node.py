# Copyright 2018, Erlang Solutions Ltd, and S2HC Sweden AB
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import logging

from typing import Dict, Union

from pyrlang.async.base_engine import BaseEngine
from pyrlang.dist.distribution import ErlangDistribution
from pyrlang.dist.base_dist_protocol import BaseDistProtocol
from pyrlang.dist.node_opts import NodeOpts
from pyrlang.bases import BaseNode
from pyrlang.process import Process
from term.atom import Atom
from term.pid import Pid

LOG = logging.getLogger("pyrlang")


class NodeException(Exception):
    def __init__(self, msg, *args, **kwargs):
        LOG.error("NodeException: %s", msg)
        Exception.__init__(self, msg, *args, **kwargs)


class ProcessNotFoundError(NodeException):
    def __init__(self, msg, *args, **kwargs):
        LOG.error("NoProcess: %s", msg)
        Exception.__init__(self, msg, *args, **kwargs)


class Node(BaseNode):
    """ Implements an Erlang node which has a network name, a dictionary of 
        processes and registers itself via EPMD.
        Node handles the networking asynchronously.

        This is the root object of an Erlang node, it must be created first and
        must outlive all other objects it manages, for them to be accessible
        over the network.

        Example:

        1. Create an async engine adapter:
            e = GeventEngine()  # located in `import Pyrlang`

        2. Create a node class with a name and a cookie
            ``node = Pyrlang.Node("py@127.0.0.1", "COOKIE", engine=e)``

        3. Start it via the engine adapter ``e.start(node)``

        4. Now anything that you do (for example an infinite loop with
            ``e.sleep(1)`` in it, will give CPU time to the node.
    """

    all_nodes = {}  # type: Dict[str, Node]
    """ All existing Node objects indexed by node_name: str """

    def __init__(self, node_name: str, cookie: str, engine: BaseEngine) -> None:
        BaseNode.__init__(self, node_name=node_name, engine=engine)

        Node.all_nodes[node_name] = self

        self.inbox_ = engine.queue_new()
        """ Message queue based on ``gevent.Queue``. It is periodically checked
            in the ``_run`` method and the receive handler is called. """

        self.pid_counter_ = 0
        """ An internal counter used to generate unique process ids """

        # Process dictionary which stores all the existing ``Process`` objects
        # adressable by a pid.
        #
        # .. note:: This creates a python reference to an
        #     object preventing its automatic garbage collection.
        #     In the end of its lifetime an object must be explicitly removed
        #     from this dictionary using ``Process.exit`` method on the
        #     process.
        self.processes_ = {}  # type: Dict[Pid, Process]

        # Registered objects dictionary, which maps atoms to pids
        self.reg_names_ = {}  # type: Dict[Atom, Pid]

        self.is_exiting_ = False

        # An option object with feature support flags packed into an
        # integer.
        self.node_opts_ = NodeOpts(cookie=cookie)

        self.dist_nodes_ = {}  # type: Dict[str, BaseDistProtocol]
        self.dist_ = ErlangDistribution(node_name=node_name, engine=engine)

        # This is important before we can begin spawning processes
        # to get the correct node creation
        self.dist_.connect()

        # Spawn and register (automatically) the process 'rex' for remote
        # execution, which takes 'rpc:call's from Erlang
        from pyrlang.rex import Rex
        self.rex_ = Rex(self)

        # Spawn and register (automatically) the 'net_kernel' process which
        # handles special ping messages
        from pyrlang.net_kernel import NetKernel
        self.net_kernel_ = NetKernel(self)

        self.engine_.spawn(self._loop)

    def _loop(self) -> bool:
        """ Returns True to continue running. False to stop. """
        self.handle_inbox()
        return not self.is_exiting_

    def handle_inbox(self):
        while True:
            msg = self.inbox_.get()
            if msg is None:
                break
            self.handle_one_inbox_message(msg)

    def handle_one_inbox_message(self, m: tuple):
        """ Handler is called whenever a message arrives to the mailbox.
        """
        # Send a ('node_connected', NodeName, Connection) to inform about the
        # connectivity with the other node
        if m[0] == "node_connected":
            (_, addr, connection) = m
            LOG.info("Node %s connected", addr)
            self.dist_nodes_[addr] = connection

        # Send a ('node_disconnected', NodeName) to forget the connection
        elif m[0] == "node_disconnected":
            (_, addr) = m
            if addr in self.dist_nodes_:  # preventing KeyError here
                LOG.info("Node %s disconnected", addr)
                del self.dist_nodes_[addr]

    def register_new_process(self, proc=None) -> Pid:
        """ Generate a new pid and add the process to the process dictionary.

            :type proc: Process or None
            :param proc: A new process which needs a pid, or None if you only
                need a fake pid
            :return: A new pid (does not modify the process in place, so please
                store the pid!)
        """
        pid1 = Pid(node_name=self.node_name_,
                   id=self.pid_counter_ // 0x7fffffff,
                   serial=self.pid_counter_ % 0x7fffffff,
                   creation=self.dist_.creation_)
        self.pid_counter_ += 1

        if proc is not None:
            self.processes_[pid1] = proc
        return pid1

    def on_exit_process(self, exiting_pid, reason):
        LOG.info("Process %s exited with %s", exiting_pid, reason)
        del self.processes_[exiting_pid]

    def register_name(self, proc, name) -> None:
        """ Add a name into registrations table (automatically removed when the
            referenced process is removed)

            :type proc: Process
            :param proc: The process to register
            :type name: Atom
            :param name: The name to register with
        """
        self.reg_names_[name] = proc.pid_

    def where_is(self, ident) -> Union[Process, None]:
        """ Look up a registered name or pid.

            :rtype: Process or None
        """
        if isinstance(ident, Atom) and ident in self.reg_names_:
            ident = self.reg_names_[ident]

        if isinstance(ident, Pid) and ident in self.processes_:
            return self.processes_[ident]

        return None

    def _send_local_registered(self, receiver, message) -> None:
        """ Try find a named process by atom key, drop a message into its inbox_

            :param receiver: A name, atom, of the receiver process
            :param message: The message
        """
        if not isinstance(receiver, Atom):
            raise NodeException("_send_local_registered receiver must be an "
                                "atom")

        receiver_obj = self.where_is(receiver)
        if receiver_obj is not None:
            LOG.info("Send local reg=%s receiver=%s msg=%s",
                     receiver, receiver_obj, message)
            receiver_obj.deliver_message(msg=message)
        else:
            LOG.warning("Send to unknown %s ignored", receiver)

    def _send_local(self, receiver, message) -> None:
        """ Try find a process by pid and drop a message into its ``inbox_``.

            :param receiver:  Pid who will receive the message
            :param message:  The message
        """
        if not isinstance(receiver, Pid):
            raise NodeException("send's receiver must be a pid")

        dst = self.where_is(receiver)
        if dst is not None:
            LOG.debug("Node._send_local: to %s <- %s", receiver, message)
            dst.deliver_message(msg=message)
        else:
            LOG.warning("Node._send_local: receiver %s does not exist", receiver)

    def send(self, sender, receiver, message) -> None:
        """ Deliver a message to a pid or to a registered name. The pid may be
            located on another Erlang node.

            :param sender: Message sender
            :type receiver: Pid or Atom or tuple[Atom, Pid or Atom]
            :param receiver: Message receiver, a pid, or a name, or a tuple with
                node name and a receiver on the remote node.
            :param message: Any value which will be placed into the receiver
                inbox. Pyrlang processes use tuples but that is not enforced
                for your own processes.
        """
        # LOG.debug("send to %s <- %s", receiver, message)

        if isinstance(receiver, tuple):
            (r_node, r_name) = receiver
            if r_node == self.node_name_:  # atom compare
                # re-route locally
                return self.send(sender, r_name, message)
            else:
                # route remotely
                return self._send_remote(sender=sender,
                                         dst_node=str(r_node),
                                         receiver=r_name,
                                         message=message)

        elif isinstance(receiver, Pid):
            if receiver.is_local_to(self):
                return self._send_local(receiver, message)
            else:
                return self._send_remote(sender=sender,
                                         dst_node=receiver.node_name_,
                                         receiver=receiver,
                                         message=message)

        elif isinstance(receiver, Atom):
            return self._send_local_registered(receiver, message)

        raise NodeException("Don't know how to send to %s" % receiver)

    def _send_remote(self, sender, dst_node: str, receiver, message) -> None:
        # LOG.debug("send_remote to %s <- %s" % (receiver, message))
        m = ('send', sender, receiver, message)
        return self.dist_command(receiver_node=dst_node,
                                 message=m)

    def get_cookie(self):
        """ Get string cookie value for this node.
            TODO: Cookie per connection?
        """
        return self.node_opts_.cookie_

    def dist_command(self, receiver_node: str, message: tuple) -> None:
        """ Locate the connection to the given node (a string).
            Place a tuple crafted by the caller into message box for Erlang
            distribution socket. It will pick up and handle the message whenever
            possible.

            :param receiver_node: Name of a remote node
            :param message: A crafted tuple with command name and some more
                values
            :raises NodeException: if unable to find or connect to the node
        """
        if receiver_node not in self.dist_nodes_:
            LOG.info("Connect to node %s", receiver_node)
            handler = self.dist_.connect_to_node(
                local_node=self.node_name_,
                remote_node=receiver_node,
                engine=self.engine_)

            if handler is None:
                raise NodeException("Node not connected %s" % receiver_node)

            # block until connected, and get the connected message
            LOG.info("Wait for 'node_connected'")
            while receiver_node not in self.dist_nodes_:
                self.engine_.sleep(0.1)
                if self.is_exiting_:
                    return

            LOG.info("Connected")

        conn = self.dist_nodes_[receiver_node]
        conn.inbox_.put(message)

    def monitor_process(self, origin, target):
        """ Locate the process referenced by the target and place the origin
            pid into its ``monitors_`` collection. When something happens to the
            ``target``, a special message will be sent to the ``origin``.

            :type origin: Pid
            :param origin: The (possibly remote) process who will be monitoring
                the target from now
            :type target: Pid or Atom
            :param target: Name or pid of a monitor target process
            :raises ProcessNotFound: if target does not exist
       """
        target_proc = self.where_is(target)
        LOG.info("MonitorP: orig=%s targ=%s -> %s", origin, target, target_proc)
        if target_proc is not None:
            target_proc.monitors_.add(origin)
        else:
            msg = "Monitor target %s does not exist" % target
            LOG.error(msg)
            raise ProcessNotFoundError(msg)

        # if the origin is local, register monitor in it
        if origin.is_local_to(self):
            origin_p = self.where_is(origin)
            origin_p.monitor_targets_.add(target_proc.pid_)

    def demonitor_process(self, origin, target):
        """ Locate the process ``target`` and remove the ``origin`` from its
            ``monitors_`` collection. This does not trigger any notifications
            or signals to the ``origin``.

            :type origin: Pid
            :param origin: The process who was monitoring the target previously
            :type target: Pid or Atom
            :param target: Name or pid of a monitor target process, possibly
                it does not exist
            :raises ProcessNotFound: if target does not exist
        """
        target_proc = self.where_is(target)
        if target_proc is not None:
            target_proc.monitors_.discard(origin)
        else:
            msg = "Demonitor target %s does not exist" % target
            LOG.error(msg)
            raise ProcessNotFoundError(msg)

        # if the origin is local, unregister monitor from it
        if origin.is_local_to(self):
            origin_p = self.where_is(origin)
            origin_p.monitor_targets_.discard(target_proc.pid_)

    def destroy(self):
        """ Closes incoming and outgoing connections and destroys the local
            node. """
        self.is_exiting_ = True

        import copy
        all_processes = copy.copy(self.processes_)
        for p in all_processes.values():
            p.exit(Atom('killed'))
        self.processes_.clear()
        self.reg_names_.clear()

        for dproto in self.dist_nodes_.values():
            dproto.destroy()
        self.dist_nodes_.clear()

        self.dist_.destroy()
        del Node.all_nodes[self.node_name_]

        self.engine_.destroy()


__all__ = ['Node', 'NodeException', 'ProcessNotFoundError']

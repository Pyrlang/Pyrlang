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

from typing import Dict, Union, Set

from pyrlang.async_support.base_engine import BaseEngine
from pyrlang.dist.distribution import ErlangDistribution
from pyrlang.dist.base_dist_protocol import BaseDistProtocol
from pyrlang.dist.distflags import NodeOpts
from pyrlang.bases import BaseNode, NodeDB
from pyrlang.process import Process
import term
from term.atom import Atom
from term.pid import Pid
from term.reference import Reference

LOG = logging.getLogger(__name__)


class NodeException(Exception):
    def __init__(self, msg, *args, **kwargs):
        LOG.error("NodeException: %s", msg)
        Exception.__init__(self, msg, *args, **kwargs)


class ProcessNotFoundError(NodeException):
    def __init__(self, msg, *args, **kwargs):
        LOG.error("NoProcess: %s", msg)
        Exception.__init__(self, msg, *args, **kwargs)


class BadArgError(Exception):
    def __init__(self, msg, *args, **kwargs):
        LOG.error("Bad Argument: %s", msg)
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

    node_db = NodeDB()

    def __init__(self, node_name: str, cookie: str, engine: BaseEngine) -> None:
        BaseNode.__init__(self, node_name=node_name, engine=engine)

        # register node to db
        self.node_db.register(self)

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

        self._signal_wakeups = set()  # type: Set[Pid]

        self.dist_nodes_ = {}  # type: Dict[str, BaseDistProtocol]
        self.dist_ = ErlangDistribution(node_name=node_name, engine=engine)

        # This is important before we can begin spawning processes
        # to get the correct node creation
        self.dist_.connect()

        # Spawn and register (automatically) the process 'rex' for remote
        # execution, which takes 'rpc:call's from Erlang
        from pyrlang.rex import Rex
        self.rex_ = Rex()

        # Spawn and register (automatically) the 'net_kernel' process which
        # handles special ping messages
        from pyrlang.net_kernel import NetKernel
        self.net_kernel_ = NetKernel(node=self)

        self.engine_.spawn(self._loop)

    def _loop(self) -> bool:
        """ Returns True to continue running. False to stop. """
        self._handle_inbox()
        self._handle_signals()
        return not self.is_exiting_

    def _handle_signals(self):
        """ For all knowns processes which might have a pending signal waiting,
            give CPU time to that process. """
        while self._signal_wakeups:
            s_pid = self._signal_wakeups.pop()
            if s_pid in self.processes_:
                self.processes_[s_pid].handle_signals()

    def signal_wake_up(self, pid):
        """ Process informs Node that it now has a signal in its signals queue.
            Try and process it when possible. """
        self._signal_wakeups.add(pid)

    def _handle_inbox(self):
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

    def where_is_process(self, ident):
        """ Look up a registered name or pid.

            :rtype: pyrlang.process.Process or None
        """
        if isinstance(ident, Atom) and ident in self.reg_names_:
            ident = self.reg_names_[ident]

        if isinstance(ident, Pid) and ident in self.processes_:
            return self.processes_[ident]

        return None

    def where_is(self, ident):
        """ Look up a registered name or pid.

            :param ident: an Atom or a Pid to convert to a Pid
            :type ident: term.atom.Atom or term.pid.Pid
            :rtype: term.pid.Pid
        """
        if isinstance(ident, Atom) and ident in self.reg_names_:
            return self.reg_names_[ident]

        if isinstance(ident, Pid):
            return ident

        raise BadArgError("where_is argument must be a pid or an atom (%s)" % ident)

    def _send_local_registered(self, receiver, message) -> None:
        """ Try find a named process by atom key, drop a message into its inbox_

            :param receiver: A name, atom, of the receiver process
            :param message: The message
        """
        if not isinstance(receiver, Atom):
            raise NodeException("_send_local_registered receiver must be an "
                                "atom")

        receiver_obj = self.where_is_process(receiver)
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

        dst = self.where_is_process(receiver)
        if dst is not None:
            LOG.debug("Node._send_local: to %s <- %s", receiver, message)
            dst.deliver_message(msg=message)
        else:
            LOG.warning("Node._send_local: receiver %s does not exist", receiver)

    def send(self, sender, receiver, message) -> None:
        """ Deliver a message to a pid or to a registered name. The pid may be
            located on another Erlang node.

            :param sender: Message sender
            :type sender: term.pid.Pid or None
            :type receiver: term.pid.Pid or term.atom.Atom or Tuple[Atom, Pid or Atom]
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
        return self._dist_command(receiver_node=dst_node,
                                  message=m)

    def get_cookie(self):
        """ Get string cookie value for this node.
            TODO: Cookie per connection?
        """
        return self.node_opts_.cookie_

    def _dist_command(self, receiver_node: str, message: tuple) -> None:
        """ Locate the connection to the given node (a string).
            Place a tuple crafted by the caller into message box for Erlang
            dist_proto socket. It will pick up and handle the message whenever
            possible.

            :param receiver_node: Name of a remote node
            :param message: A crafted tuple with command name and some more
                values
            :raises NodeException: if unable to find or connect to the node
        """
        if self.is_exiting_:
            LOG.warning("Ignored dist command %s (node is exiting)" % (message,))
            return

        if receiver_node not in self.dist_nodes_:
            LOG.info("Connect to node %s", receiver_node)
            handler = self.dist_.connect_to_node(
                local_node=self.node_name_,
                remote_node=receiver_node,
                engine=self.engine_)

            if handler is None:
                raise NodeException("Node %s not connected (1)" % receiver_node)

            # block until connected, and get the connected message
            LOG.info("Wait for 'node_connected'")
            while receiver_node not in self.dist_nodes_:
                self.engine_.sleep(0.1)
                if self.is_exiting_:
                    return

            LOG.info("Connected")

        conn = self.dist_nodes_.get(receiver_node, None)
        if conn is None:
            raise NodeException("Node %s is not connected (2)" % receiver_node)
        else:
            conn.inbox_.put(message)

    def link(self, pid1, pid2, local_only=False):
        """ Check each of processes pid1 and pid2 if they are local, mutually
            link them. Assume remote process handles its own linking.

            :param pid1: First pid
            :type pid1: term.pid.Pid
            :param pid2: Second pid
            :type pid2: term.pid.Pid
            :param local_only: If set to True, linking to remote pids will send
                LINK message over dist_proto protocol
        """
        if pid1.is_local_to(self):
            if pid1 in self.processes_:
                self.processes_[pid1].add_link(pid2)
            else:
                # not exists
                self.send_exit_signal(pid2, pid1, Atom("noproc"))

        elif not local_only:
            link_m = ('link', pid2, pid1)
            self._dist_command(receiver_node=pid1.node_name_, message=link_m)

        if pid2.is_local_to(self):
            if pid2 in self.processes_:
                self.processes_[pid2].add_link(pid1)
            else:
                # not exists
                self.send_exit_signal(pid1, pid2, Atom("noproc"))

        elif not local_only:
            link_m = ('link', pid1, pid2)
            self._dist_command(receiver_node=pid2.node_name_, message=link_m)

    def unlink(self, pid1, pid2, local_only=False):
        """ Mutually unlink two processes.

            :param pid1: First pid
            :type pid1: term.pid.Pid
            :param pid2: Second pid
            :type pid2: term.pid.Pid
            :param local_only: If set to True, linking to remote pids will send
                UNLINK message over dist_proto protocol
        """
        if pid1.is_local_to(self):
            self.processes_[pid1].remove_link(pid2)
        elif not local_only:
            link_m = ('unlink', pid2, pid1)  # (unlink, localpid, remotepid)
            self._dist_command(receiver_node=pid1.node_name_, message=link_m)

        if pid2.is_local_to(self):
            self.processes_[pid2].remove_link(pid1)
        elif not local_only:
            link_m = ('unlink', pid1, pid2)  # (unlink, localpid, remotepid)
            self._dist_command(receiver_node=pid2.node_name_, message=link_m)

    def monitor_process(self, origin_pid: Pid, target, ref=None):
        """ Locate the process referenced by the target and place the origin
            pid into its ``monitors_`` collection. When something happens to the
            ``target``, a special message will be sent to the ``origin``.
            Remote targets are supported.

            :param ref: If not None, will be reused, else a new random ref will
                be generated.
            :type ref: None or term.reference.Reference
            :type origin_pid: term.pid.Pid
            :param origin_pid: The (possibly remote) process who will be monitoring
                the target from now and wants to know when we exit.
            :type target: term.pid.Pid or term.atom.Atom
            :param target: Name or pid of a monitor target process.
            :rtype: term.reference.Reference
            :raises pyrlang.node.ProcessNotFoundError: if target does not exist.
        """
        target_pid = self.where_is(target)
        m_ref = ref if ref is not None \
            else Reference.create(node_name=self.node_name_,
                                  creation=self.dist_.creation_)

        if not origin_pid.is_local_to(self):
            # Origin is remote and wants to monitor local process
            return self._monitor_local_process(origin_pid, target_pid, m_ref)
        if target_pid.is_local_to(self):
            # Target is local and we notify a local process
            return self._monitor_local_process(origin_pid, target_pid, m_ref)
        # Target is remote and a dist_proto message has to be sent
        return self._monitor_remote_process(origin_pid, target_pid, m_ref)

    def _monitor_remote_process(self, origin_pid: Pid, target_pid: Pid,
                                ref: Reference):
        monitor_msg = ('monitor_p', origin_pid, target_pid, ref)
        self._dist_command(receiver_node=target_pid.node_name_,
                           message=monitor_msg)

        # if the origin is local, register monitor in it. Remote pids are
        # handled by the remote
        assert origin_pid.is_local_to(self)
        origin_p = self.where_is_process(origin_pid)
        origin_p.add_monitor(pid=target_pid, ref=ref)

    def _monitor_local_process(self, origin_pid: Pid, target_pid: Pid,
                               ref: Reference):
        """ Monitor a local target. """
        target_proc = self.processes_.get(target_pid, None)
        # LOG.info("Monitor: orig=%s targ=%s -> %s", origin, target, target_proc)

        if target_proc is not None:
            target_proc.add_monitored_by(pid=origin_pid, ref=ref)
        else:
            msg = "Monitor target %s does not exist" % target_pid
            LOG.error(msg)
            raise ProcessNotFoundError(msg)

        # if the origin is local, register monitor in it.
        # Remotely set monitors have remote origin
        if origin_pid.is_local_to(self):
            origin_p = self.where_is_process(origin_pid)
            origin_p.add_monitor(pid=target_proc.pid_, ref=ref)

    def demonitor_process(self, origin_pid, target, ref):
        """ Locate the process ``target`` and remove the ``origin`` from its
            ``monitors_`` collection. This does not trigger any notifications
            or signals to the ``origin``.

            :param ref: Reference which you received when setting up a monitor.
            :type ref: term.reference.Reference
            :type origin_pid: Pid
            :param origin_pid: The process who was monitoring the target previously
            :type target: Pid or Atom
            :param target: Name or pid of a monitor target process, possibly
                it does not exist
            :raises ProcessNotFound: if target does not exist
        """
        target_pid = self.where_is(target)

        LOG.info("demonitor orig=%s target=%s ref=%s", origin_pid, target_pid, ref)

        if not origin_pid.is_local_to(self):
            # Remote node monitored us but now wants to release
            return self._demonitor_local_process(origin_pid, target_pid, ref=ref)
        if target_pid.is_local_to(self):
            # Target process is local for this node
            return self._demonitor_local_process(origin_pid, target_pid, ref=ref)
        # Target process is remote, and we need to send monitor message
        return self._demonitor_remote_process(origin_pid, target_pid, ref=ref)

    def _demonitor_remote_process(self, origin_pid: Pid, target_pid: Pid,
                                  ref: Reference):
        monitor_msg = ('demonitor_p', origin_pid, target_pid, ref)
        self._dist_command(receiver_node=target_pid.node_name_,
                           message=monitor_msg)

        origin_p = self.where_is_process(origin_pid)
        origin_p.remove_monitored_by(pid=target_pid, ref=ref)

    def _demonitor_local_process(self, origin_pid: Pid, target_pid: Pid,
                                 ref: Reference):
        target_proc = self.processes_.get(target_pid, None)
        if target_proc is not None:
            target_proc.remove_monitored_by(ref=ref, pid=origin_pid)
        else:
            msg = "Demonitor target %s does not exist" % target_pid
            LOG.error(msg)
            raise ProcessNotFoundError(msg)

        # if the origin is local, unregister monitor from it
        # Remotely set monitors have remote origin
        if origin_pid.is_local_to(self):
            origin_p = self.where_is_process(origin_pid)
            origin_p.remove_monitor(ref=ref, pid=target_pid)

    def destroy(self):
        """ Closes incoming and outgoing connections and destroys the local
            node. This is Python, so some refs from running async handlers
            may remain.
        """
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
        self.node_db.remove(self)

        self.engine_.destroy()
        del self

    def exit_process(self, sender, receiver, reason):
        """ Delivers exit message to a local or remote process. """
        self._send_exit_signal(sender=sender,
                               receiver=receiver,
                               reason=reason,
                               dist_protocol_message='exit2')

    def send_link_exit_notification(self, sender, receiver, reason):
        """ Delivers exit message due to a linked process dead to a local
            or remote process. """
        self._send_exit_signal(sender=sender,
                               receiver=receiver,
                               reason=reason,
                               dist_protocol_message='exit')

    def _send_exit_signal(self, sender, receiver, reason,
                          dist_protocol_message: str = 'exit'):
        """ Deliver local or remote exit signal to a process.

            :param dist_protocol_message: Defines message which is sent to the
                dist_proto protocol and then converted to an integer. EXIT is
                used for link exits, and EXIT2 is used for triggering remote exits.
        """
        if receiver.is_local_to(self):
            recvp = self.processes_.get(receiver, None)
            if recvp is None:
                return  # can't kill that which doesn't exist
            recvp.exit(reason=reason)
        else:
            # This is a remote Pid, so send something remotely
            distm = (dist_protocol_message, sender, receiver, reason)
            self._dist_command(receiver_node=receiver.node_name_,
                               message=distm)


__all__ = ['Node', 'NodeException', 'ProcessNotFoundError']

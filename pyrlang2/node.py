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
import asyncio
import logging
from typing import Dict, Set

from pyrlang2.dist_proto import DistributionFlags, ErlangDistribution
from pyrlang2.errors import BadArgError, NodeException
from term import Pid, Atom

LOG = logging.getLogger("pyrlang")


class Node:
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
    """ All existing local Node objects indexed by node_name: str """

    def __init__(self, node_name: str, cookie: str) -> None:
        """ Sets up the new node, initiating EPMD connection as necessary
            :param node_name: str
            :param cookie: str
        """
        self.node_name_ = node_name  # type: str
        """ Node name as seen on the network. Use full node names here:
            ``name@hostname`` """

        Node.all_nodes[node_name] = self

        self.inbox_ = asyncio.Queue()
        """ Message queue is awaited, and the receive handler is called
            if anything arrives. """

        self.pid_counter_ = 0
        """ An internal counter used to generate unique process ids """

        self.processes_ = {}  # type: Dict[Pid, Process]
        """ Process dictionary which stores all the existing ``Process`` objects
            adressable by a pid.
    
            .. note:: This creates a python reference to an
                object preventing its automatic garbage collection.
                In the end of its lifetime an object must be explicitly removed
                from this dictionary using ``Process.exit`` method on the
                process.
        """

        self.reg_names_ = {}  # type: Dict[Atom, Pid]
        """ Registered objects dictionary, which maps atoms to pids
        """

        self.is_exiting_ = False

        self.node_opts_ = DistributionFlags(cookie=cookie)
        """ Distribution options object with feature support flags packed into 
            an integer. The remote node will receive these flags to know what
            features we can support.
        """

        self._signal_wakeups = set()  # type: Set[Pid]

        self.dist_nodes_ = {}  # type: Dict[str, BaseDistProtocol]
        self.dist_ = ErlangDistribution(node_name=node_name)

        # Spawn and register (automatically) the process 'rex' for remote
        # execution, which takes 'rpc:call's from Erlang
        # from pyrlang2.rex import Rex
        # self.rex_ = Rex(node=self)

        # Spawn and register (automatically) the 'net_kernel' process which
        # handles special ping messages
        # from pyrlang2.net_kernel import NetKernel
        # self.net_kernel_ = NetKernel(node=self)

        asyncio.get_event_loop().create_task(self._async_loop())

    async def _async_loop(self):
        # This is important before we can begin spawning processes
        # to get the correct node creation
        await self.dist_.start_distribution()

        LOG.info("Node async_loop ended")

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

        raise BadArgError(
            "where_is argument must be a pid or an atom (%s)" % ident)

    async def send(self, sender, receiver, message) -> None:
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
                return await self.send(sender, r_name, message)
            else:
                # route remotely
                return await self._send_remote(sender=sender,
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
            LOG.warning("Node._send_local: receiver %s does not exist",
                        receiver)

    async def _send_remote(self, sender, dst_node: str, receiver,
                           message) -> None:
        # LOG.debug("send_remote to %s <- %s" % (receiver, message))
        m = ('send', sender, receiver, message)
        return await self._dist_command(receiver_node=dst_node,
                                        message=m)

    def get_cookie(self):
        """ Get string cookie value for this node.
            TODO: Cookie per connection?
        """
        return self.node_opts_.cookie_

    async def _dist_command(self, receiver_node: str, message: tuple) -> None:
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
            LOG.warning(
                "Ignored dist command %s (node is exiting)" % (message,))
            return

        if receiver_node not in self.dist_nodes_:
            LOG.info("Connect to node %s", receiver_node)
            handler = await self.dist_.connect_to_node(
                local_node=self.node_name_,
                remote_node=receiver_node
            )

            if handler is None:
                raise NodeException("Node %s not connected (1)" % receiver_node)

            # block until connected, and get the connected message
            LOG.info("Wait for 'node_connected'")
            while receiver_node not in self.dist_nodes_:
                await asyncio.sleep(0.1)
                if self.is_exiting_:
                    return

            LOG.info("Connected")

        conn = self.dist_nodes_.get(receiver_node, None)
        if conn is None:
            raise NodeException("Node %s is not connected (2)" % receiver_node)
        else:
            conn.inbox_.put(message)

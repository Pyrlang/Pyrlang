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
import sys
from typing import Dict, Set

from pyrlang.dist_proto import DistributionFlags, ErlangDistribution
from pyrlang.dist_proto.base_dist_protocol import BaseDistProtocol
from pyrlang.net_kernel import NetKernel
from pyrlang.errors import BadArgError, NodeException, ProcessNotFoundError
from pyrlang.node_db import NodeDB
from pyrlang.process import Process
from pyrlang.rex import Rex
from term import Pid, Atom, Reference

LOG = logging.getLogger(__name__)


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

    node_db = NodeDB()
    """ All existing local Node objects indexed by node_name: str """

    def __init__(self, node_name: str, cookie: str,
                 hidden: bool = False) -> None:
        """ Sets up the new node, initiating EPMD connection as necessary
            :param node_name: str
            :param cookie: str
            :param hidden: bool - set to True if you want node to be hidden
                (i.e. not visible in remote ``nodes().``)
        """
        self.node_name_ = node_name  # type: str
        """ Node name as seen on the network. Use full node names here:
            ``name@hostname`` """

        self.node_db.register(self)

        self.inbox_ = asyncio.Queue()  # type: asyncio.Queue
        """ Contains Pyrlang's own messages to the local node. """

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
        if not hidden:
            self.node_opts_.set_node_published()

        self._signal_wakeups = set()  # type: Set[Pid]

        self.dist_nodes_ = {}  # type: Dict[str, BaseDistProtocol]
        self.dist_ = ErlangDistribution(node_name=node_name)

        # Spawn and register (automatically) the process 'rex' for remote
        # execution, which takes 'rpc:call's from Erlang
        self.rex_ = Rex()

        # Spawn and register (automatically) the 'net_kernel' process which
        # handles special ping messages
        self.net_kernel_ = NetKernel()

        self._event_loop = asyncio.get_event_loop()
        self._event_loop.create_task(self._async_loop())
        # future object we're awaiting when running the loop for the node
        self.__completed_future = self._event_loop.create_future()

        self._prev_unlink_identifier = -1

    async def _async_loop(self):
        # This is important before we can begin spawning processes
        # to get the correct node creation
        await self.dist_.start_distribution()

        while not self.is_exiting_:
            await asyncio.sleep(1)

        # LOG.info("Node async_loop ended")

    async def handle_signals(self):
        while self._signal_wakeups:
            s_pid = self._signal_wakeups.pop()
            if s_pid not in self.processes_:
                LOG.info("got signal for untracked process %s", s_pid)
                continue
            await self.processes_[s_pid].handle_signals()

    def signal_wake_up(self, pid):
        self._signal_wakeups.add(pid)
        self._event_loop.create_task(self.handle_signals())

    def on_exit_process(self, exiting_pid, reason):
        LOG.info("Process %s exited with %s", exiting_pid, reason)
        del self.processes_[exiting_pid]

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

    def send_nowait(self, sender, receiver, message) -> None:
        """ Create task that sends the message
        """
        send_task = self.send(sender, receiver, message)
        self._event_loop.create_task(send_task)

    async def send(self, sender, receiver, message) -> None:
        """ Deliver a message to a pid or to a registered name. The pid may be
            located on another Erlang node.

            :param sender: Message sender
            :type sender: term.pid.Pid or None
            :type receiver: term.pid.Pid or term.atom.Atom or Tuple[Atom, Pid or
                Atom]
            :param receiver: Message receiver, a pid, or a name, or a tuple with
                node name and a receiver on the remote node.
            :param message: Any value which will be placed into the receiver
                inbox. Pyrlang processes use tuples but that is not enforced
                for your own processes.
        """
        LOG.debug("send to %s <- %s", receiver, message)

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
                return await self._send_remote(sender=sender,
                                               dst_node=receiver.node_name_,
                                               receiver=receiver,
                                               message=message)

        elif isinstance(receiver, Atom):
            return self._send_local_registered(receiver, message)

        raise NodeException("Don't know how to send to %s" % receiver)

    def register_dist_node(self, addr, proto):
        """
        add the protocol to dist nodes so that we can use it
        :param addr:
        :param proto:
        :return:
        """
        self.dist_nodes_[addr] = proto

    def unregister_dist_node(self, addr):
        """
        remove dist node (disconnected most likley)
        :param addr:
        :return:
        """
        if addr not in self.dist_nodes_:
            return

        self.dist_nodes_.pop(addr)

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
        LOG.debug("send_remote to %s <- %s" % (receiver, message))
        m = ('send', sender, receiver, message)
        return await self.dist_command(receiver_node=dst_node,
                                       message=m)

    def get_cookie(self):
        """ Get string cookie value for this node.
            TODO: Cookie per connection?
        """
        return self.node_opts_.cookie_

    async def dist_command(self, receiver_node: str, message: tuple) -> None:
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
            LOG.warning("Ignored dist command %s (node is exiting)", message)
            return

        await self._establish_connection(receiver_node)

        conn = self.dist_nodes_.get(receiver_node, None)
        if conn is None:
            raise NodeException("Node %s is not connected (2)" % receiver_node)
        else:
            conn.inbox_.put_nowait(message)

    async def _establish_connection(self, receiver_node):
        """Establish a distribution connection to receiver_node."""

        if receiver_node not in self.dist_nodes_:
            LOG.info("send_remote: Node %s is not connected. Trying...",
                     receiver_node)
            handler = await self.dist_.connect_to_node(
                local_node=self.node_name_,
                remote_node=receiver_node
            )

            if handler is None:
                raise NodeException("Node %s not connected (1)" % receiver_node)

            # block until connected, and get the connected message
            LOG.info("Wait for 'node_connected' from my distribution protocol")
            while receiver_node not in self.dist_nodes_:
                await asyncio.sleep(0.1)
                if self.is_exiting_:
                    return

            LOG.info("Connected to %s", receiver_node)

    def link_nowait(self, pid1, pid2, local_only=False):
        """ unsafe casting of link

            you can't assume it has got effect when this function returns
            Convinience methot for being able to call it outside a async
            function
         """
        link_task = self.link(pid1, pid2, local_only)
        self._event_loop.create_task(link_task)

    async def link(self, from_pid, to_pid, local_only=False):
        """ Process a link event generated by from_pid, targeting to_pid.

            :param from_pid: Source pid
            :type from_pid: term.pid.Pid
            :param to_pid: Target pid
            :type to_pid: term.pid.Pid
            :param local_only: Deprecated, ignored.
        """
        if from_pid.is_local_to(self) and from_pid not in self.processes_:
            LOG.info("local from_pid %s not found during link", from_pid)
            return
        if to_pid.is_local_to(self) and to_pid not in self.processes_:
            await self._send_exit_signal(to_pid, from_pid, Atom("noproc"))
            return

        if from_pid.is_local_to(self) and to_pid.is_local_to(self):
            from_process = self.processes_[from_pid]
            to_process = self.processes_[to_pid]
            from_process._set_link_state(to_pid, True)
            to_process._set_link_state(from_pid, True)

        elif from_pid.is_local_to(self):  # we're generating the message
            from_process = self.processes_[from_pid]
            link_exists = from_process._link_state(to_pid)
            if link_exists is True:
                return

            from_process._set_link_state(to_pid, True)
            await self.dist_command(receiver_node=to_pid.node_name_,
                                    message=('link', from_pid, to_pid))

        elif to_pid.is_local_to(self):
            to_process = self.processes_[to_pid]
            if to_process._link_state(from_pid) is not None:
                return
            to_process._set_link_state(from_pid, True)

        else:
            LOG.warn("Illegal remote-remote link: %s -> %s", from_pid, to_pid)

    async def unlink(self, from_pid, to_pid, local_only=False):
        """ Mutually unlink two processes.

            :param from_pid: First pid
            :type from_pid: term.pid.Pid
            :param to_pid: Second pid
            :type to_pid: term.pid.Pid
            :param local_only: If set to True, linking to remote pids will send
                UNLINK message over dist_proto protocol
        """

        if from_pid.is_local_to(self) and to_pid.is_local_to(self):
            if from_pid not in self.processes_:
                return
            from_process = self.processes_[from_pid]
            if to_pid not in self.processes_:
                return
            to_process = self.processes_[to_pid]

            from_process._set_link_state(to_pid, None)
            to_process._set_link_state(from_pid, None)

            return

        if to_pid.is_local_to(self):  # inbound old-protocol message
            if to_pid not in self.processes_:
                return

            to_process = self.processes_[to_pid]
            to_process._set_link_state(from_pid, None)

            return

        # otherwise, this is an outbound request. start by performing protocol selection
        await self._establish_connection(to_pid.node_name_)
        conn = self.dist_nodes_.get(to_pid.node_name_, None)
        if conn is None:
            raise NodeException("Node %s is not connected (2)" % to_pid.node_name_)

        # at this point we're done awaiting stuff, so it's safe to unpack processes
        if from_pid not in self.processes_:
            return
        from_process = self.processes_[from_pid]
        if from_process._link_state(to_pid) != True:
            return

        from .dist_proto.flags import DFLAG_UNLINK_ID
        if conn.peer_flags_ & DFLAG_UNLINK_ID == 0:  # old protocol
            from_process._set_link_state(to_pid, None)
            return await self.dist_command(to_pid.node_name_,
                                           ('unlink', from_pid, to_pid))
        else:  # new protocol
            self._prev_unlink_identifier += 1
            identifier = self._prev_unlink_identifier
            from_process._set_link_state(to_pid, identifier)  # set to "unlinking"
            return await self.dist_command(to_pid.node_name_,
                                           ('unlink_id', identifier, from_pid, to_pid))

    async def _unlink_id(self, identifier, from_pid, to_pid):
        """ Services an UNLINK_ID message arriving over distribution.

            NOTE: The ERTS manual says: "Upon reception of an UNLINK_ID signal,
                  the corresponding UNLINK_ID_ACK signal must be sent before any
                  other signals are sent to the sender of the UNLINK_ID signal."
                  It's unclear that `await self.dist_command` satisfies this.
        """

        if to_pid not in self.processes_:
            return
        to_process = self.processes_[to_pid]
        if to_process._link_state(from_pid) == True:
            to_process._set_link_state(from_pid, None)

        # NOTE: to_pid / from_pid are deliberately swapped in the response
        return await self.dist_command(from_pid.node_name_,
                                       ('unlink_id_ack', identifier, to_pid, from_pid))

    async def _unlink_id_ack(self, identifier, from_pid, to_pid):
        """ Services an UNLINK_ID_ACK message arriving over distribution.
        """

        if to_pid not in self.processes_:
            return
        to_process = self.processes_[to_pid]
        if to_process._link_state(from_pid) == identifier:
            to_process._set_link_state(from_pid, None)

    def monitor_process(self, origin_pid: Pid, target, ref=None):
        """ Locate the process referenced by the target and place the origin
            pid into its ``monitors_`` collection. When something happens to the
            ``target``, a special message will be sent to the ``origin``.
            Remote targets are supported.

            :param ref: If not None, will be reused, else a new random ref will
                be generated.
            :type ref: None or term.reference.Reference
            :type origin_pid: term.pid.Pid
            :param origin_pid: The (possibly remote) process who will be
                monitoring the target from now and wants to know when we exit.
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
        dc_task = self.dist_command(receiver_node=target_pid.node_name_,
                                    message=monitor_msg)
        self._event_loop.create_task(dc_task)

        # if the origin is local, register monitor in it. Remote pids are
        # handled by the remote
        assert origin_pid.is_local_to(self)
        origin_p = self.where_is_process(origin_pid)
        origin_p.add_monitor(pid=target_pid, ref=ref)
        return ref

    def _monitor_local_process(self, origin_pid: Pid, target_pid: Pid,
                               ref: Reference):
        """ Monitor a local target. """
        target_proc = self.processes_.get(target_pid, None)

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
        return ref

    async def demonitor_process(self, origin_pid, target, ref):
        """ Locate the process ``target`` and remove the ``origin`` from its
            ``monitors_`` collection. This does not trigger any notifications
            or signals to the ``origin``.

            :param ref: Reference which you received when setting up a monitor.
            :type ref: term.reference.Reference
            :type origin_pid: Pid
            :param origin_pid: The process who was monitoring the target before
            :type target: Pid or Atom
            :param target: Name or pid of a monitor target process, possibly
                it does not exist
            :raises ProcessNotFound: if target does not exist
        """
        target_pid = self.where_is(target)

        LOG.debug("demonitor orig=%s target=%s ref=%s", origin_pid, target_pid,
                  ref)

        if not origin_pid.is_local_to(self):
            # Remote node monitored us but now wants to release
            return self._demonitor_local_process(origin_pid, target_pid,
                                                 ref=ref)
        if target_pid.is_local_to(self):
            # Target process is local for this node
            return self._demonitor_local_process(origin_pid, target_pid,
                                                 ref=ref)
        # Target process is remote, and we need to send monitor message
        return self._demonitor_remote_process(origin_pid, target_pid, ref=ref)

    async def _demonitor_remote_process(self, origin_pid: Pid, target_pid: Pid,
                                        ref: Reference):
        monitor_msg = ('demonitor_p', origin_pid, target_pid, ref)
        await self.dist_command(receiver_node=target_pid.node_name_,
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
        self.node_db.remove(self.node_name_)
        self.__completed_future.set_result(True)
        del self

    def exit_process(self, sender, receiver, reason):
        """ Delivers exit message to a local or remote process. """

        exit_task = self._send_exit_signal(sender=sender,
                                           receiver=receiver,
                                           reason=reason,
                                           dist_protocol_message='exit2')
        self._event_loop.create_task(exit_task)

    def send_link_exit_notification(self, sender, receiver, reason):
        link_exit_task = self._send_link_exit_notification(sender,
                                                           receiver,
                                                           reason)
        self._event_loop.create_task(link_exit_task)

    async def _send_link_exit_notification(self, sender, receiver, reason):
        """ Delivers exit message due to a linked process dead to a local
            or remote process. """
        await self._send_exit_signal(sender=sender,
                                     receiver=receiver,
                                     reason=reason,
                                     dist_protocol_message='exit')

    async def _send_exit_signal(self, sender, receiver, reason,
                                dist_protocol_message: str = 'exit'):
        """ Deliver local or remote exit signal to a process.

            :param dist_protocol_message: Defines message which is sent to the
                dist_proto protocol and then converted to an integer. EXIT is
                used for link exits, and EXIT2 is used for triggering remote
                exits.
        """
        if receiver.is_local_to(self):
            recvp = self.processes_.get(receiver, None)
            if recvp is None:
                return  # can't kill that which doesn't exist
            recvp.exit(reason=reason)
        else:
            # This is a remote Pid, so send something remotely
            distm = (dist_protocol_message, sender, receiver, reason)
            await self.dist_command(receiver_node=receiver.node_name_,
                                    message=distm)

    def get_loop(self):
        return self._event_loop

    def run(self):
        self._event_loop.run_until_complete(self.__completed_future)

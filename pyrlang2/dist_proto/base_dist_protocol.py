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

""" Base abstract Distribution connection class
"""
import asyncio
import logging
import struct
from hashlib import md5
from typing import Union, Tuple

from pyrlang.bases import NodeDB
from pyrlang2.errors import DistributionError
from term import codec
from term import util
from term.atom import Atom

LOG = logging.getLogger("pyrlang.dist")

# Distribution protocol delivers pairs of (control_term, message).
# http://erlang.org/doc/apps/erts/erl_dist_protocol.html
# First element of control term in a 'p' message defines what it is
CONTROL_TERM_LINK = 1
CONTROL_TERM_SEND = 2
CONTROL_TERM_EXIT = 3
CONTROL_TERM_UNLINK = 4  # TODO

CONTROL_TERM_NODE_LINK = 5  # Deprecated, can be later reused by BEAM VM

CONTROL_TERM_REG_SEND = 6
CONTROL_TERM_GROUP_LEADER = 7  # TODO
CONTROL_TERM_EXIT2 = 8

CONTROL_TERM_SEND_TT = 12  # TODO
CONTROL_TERM_EXIT_TT = 13  # TODO
CONTROL_TERM_REG_SEND_TT = 16  # TODO
CONTROL_TERM_EXIT2_TT = 18  # TODO

CONTROL_TERM_MONITOR_P = 19
CONTROL_TERM_DEMONITOR_P = 20
CONTROL_TERM_MONITOR_P_EXIT = 21

CONTROL_TERM_SEND_SENDER = 22
CONTROL_TERM_SEND_SENDER_TT = 23


class BaseDistProtocol(asyncio.Protocol):
    """ Defines Erlang distribution protocol (shared parts).
        Concrete implementations for incoming (DistServerProtocol) and outgoing
        (DistClientProtocol) are located in the corresponding modules.
    """

    #
    # Used by both Incoming and Outgoing protocols
    #
    DISCONNECTED = 'disconn'
    CONNECTED = 'conn'

    #
    # Used by Incoming protocol only
    #
    RECV_NAME = 'recvname'
    WAIT_CHALLENGE_REPLY = 'wait_ch_reply'

    #
    # Used by Outgoing protocol only
    #

    RECV_STATUS = 'recv_status'
    # State 'alive' means that this connection is duplicate, next message
    # may be 'true' to allow using this new connection or 'false', requesting
    # this connection to be closed
    ALIVE = 'alive'
    RECV_CHALLENGE = 'recv_challenge'
    RECV_CHALLENGE_ACK = 'recv_challenge_ack'

    node_db = NodeDB()

    def __init__(self, node_name: str):
        """ Create connection handler object. """
        super().__init__()

        self.node_name_ = node_name
        """ Name of the running Erlang node. """

        self.packet_len_size_ = 2
        """ Packet size header is variable, 2 bytes before handshake is finished
            and 4 bytes afterwards. """

        self.addr_ = None  # type: [None, Tuple[str, int]]

        self.inbox_ = asyncio.Queue()
        """ Inbox is used to ask the connection to do something. """

        self.peer_distr_version_ = (None, None)  # type: (int, int)
        """ Protocol version range supported by the remote peer. Erlang/OTP 
            versions 19-20 supports protocol version 7, older Erlangs down to 
            R6B support version 5. """

        self.peer_flags_ = 0
        self.peer_name_ = None  # type: Union[None, str]
        self.my_challenge_ = None

        self.state_ = self.DISCONNECTED
        """ FSM state for the protocol state-machine. """

        from pyrlang2.node import Node
        self.node_class_ = Node

        self.transport_ = None  # type: [None, asyncio.Transport]
        self.unconsumed_data_ = b''

        # Ping the remote periodically if our state is CONNECTED
        self._schedule_periodic_ping_remote()

    def destroy(self):
        if self.transport_ is not None:
            self.transport_.close()
            self.transport_ = None

    def connection_made(self, transport: asyncio.Transport):
        """ Connection has been accepted and established (callback).
        """
        sock = transport.get_extra_info('socket')
        self.transport_ = transport
        self.addr_ = sock.getpeername()
        self.state_ = self.RECV_NAME

    def data_received(self, data: bytes) -> None:
        self.unconsumed_data_ += data
        if len(self.unconsumed_data_) < self.packet_len_size_:
            # Not ready yet, keep reading
            return

        # Dist protocol switches from 2 byte packet length to 4 at some point
        if self.packet_len_size_ == 2:
            pkt_size = util.u16(self.unconsumed_data_, 0)
            offset = 2
        else:
            pkt_size = util.u32(self.unconsumed_data_, 0)
            offset = 4

        if len(self.unconsumed_data_) < self.packet_len_size_ + pkt_size:
            # Length is already visible but the data is not here yet
            return

        packet = self.unconsumed_data_[offset:(offset + pkt_size)]

        # LOG.info("in %d: %s", len(packet), packet)
        if self.on_packet(packet):
            self.unconsumed_data_ = self.unconsumed_data_[(offset + pkt_size):]
            return

        # Protocol error has occured and instead we return None to request
        # connection close
        return

    def on_packet(self, data: bytes) -> bool:
        raise NotImplementedError()

    def get_node(self):
        """ Use this to get access to the Pyrlang node which owns this protocol.
            :rtype: pyrlang2.node.Node
        """
        return self.node_db.get(self.node_name_)

    def connection_lost(self, _exc):
        """ Handler is called when the client has disconnected """
        self.state_ = self.DISCONNECTED

        if self.peer_name_ is not None:
            self._inform_local_node(("node_disconnected", self.peer_name_))

    def _inform_local_node(self, msg):
        self.get_node().inbox_.put_nowait(msg)

    def _send_packet2(self, content: bytes):
        """ Send a handshake-time status message with a 2 byte length prefix
        """
        # LOG.info("out %d: %s", len(content), content)
        msg = struct.pack(">H", len(content)) + content
        self.transport_.write(msg)

    def _send_packet4(self, content: bytes):
        """ Send a connection-time status message with a 4 byte length prefix
        """
        # LOG.info("out %d: %s", len(content), content)
        msg = struct.pack(">I", len(content)) + content
        self.transport_.write(msg)

    async def on_passthrough_message(self, control_term, msg_term):
        """ On incoming 'p' message with control and data, handle it.
            :raises DistributionError: when 'p' message is not a tuple
        """
        # LOG.info("Dist t=%s; control_t=%s", msg_term, control_term)

        if type(control_term) != tuple:
            raise DistributionError(
                "In a 'p' message control term must be a tuple"
            )

        ctrl_msg_type = control_term[0]

        n = self.get_node()

        if ctrl_msg_type == CONTROL_TERM_REG_SEND:
            return await n.send(sender=control_term[1],
                                receiver=control_term[3],
                                message=msg_term)

        elif ctrl_msg_type == CONTROL_TERM_SEND:
            return await n.send(sender=None,
                                receiver=control_term[2],
                                message=msg_term)

        elif ctrl_msg_type == CONTROL_TERM_LINK:
            (_, from_pid, to_pid) = control_term
            n.link(from_pid, to_pid, local_only=True)

        elif ctrl_msg_type == CONTROL_TERM_UNLINK:
            (_, from_pid, to_pid) = control_term
            n.unlink(from_pid, to_pid, local_only=True)

        elif ctrl_msg_type == CONTROL_TERM_MONITOR_P:
            (_, sender, target, ref) = control_term
            from pyrlang.node import ProcessNotFoundError
            try:
                return n.monitor_process(origin_pid=sender,
                                         target=target,
                                         ref=ref)
            except ProcessNotFoundError:
                pass

        elif ctrl_msg_type == CONTROL_TERM_DEMONITOR_P:
            (_, sender, target, ref) = control_term
            from pyrlang.node import ProcessNotFoundError
            try:
                return n.demonitor_process(origin_pid=sender, target=target,
                                           ref=ref)
            except ProcessNotFoundError:
                pass

        elif ctrl_msg_type in [CONTROL_TERM_EXIT, CONTROL_TERM_EXIT2]:
            (_, from_pid, to_pid, reason) = control_term
            if to_pid.is_local_to(n):
                n.exit_process(from_pid, to_pid, reason)

        elif ctrl_msg_type == CONTROL_TERM_MONITOR_P_EXIT:
            (_, from_pid, to_pid, ref, reason) = control_term
            if to_pid.is_local_to(n):
                down_msg = (
                    Atom("DOWN"), ref, Atom("process"), from_pid, reason)
                await n.send(sender=from_pid, receiver=to_pid,
                             message=down_msg)

        else:
            LOG.error("Unhandled 'p' message: %s; %s", control_term, msg_term)

    def periodic_check(self):
        while True:
            try:
                msg = self.inbox_.get_nowait()
                self._handle_one_inbox_message(msg)
            except asyncio.QueueEmpty:
                break

    def _periodic_ping_remote(self):
        self._schedule_periodic_ping_remote()

        if self.state_ == self.CONNECTED and self.packet_len_size_ == 4:
            self._send_packet4(b'')

    def _schedule_periodic_ping_remote(self):
        asyncio.get_event_loop().call_later(15.0, self._periodic_ping_remote)

    def _handle_one_inbox_message(self, m):
        # Send a ('send', Dst, Msg) to deliver a message to the other side
        if m[0] == 'send':
            (_, from_pid, dst, msg) = m
            ctrl = self._control_term_send(from_pid=from_pid, dst=dst)
            # LOG.info("Control msg %s; %s" % (ctrl, msg))
            return self._control_message(ctrl, msg)

        elif m[0] == 'monitor_p_exit':
            (_, from_pid, to_pid, ref, reason) = m
            ctrl = (CONTROL_TERM_MONITOR_P_EXIT,
                    from_pid, to_pid, ref, reason)
            # LOG.info("Monitor proc exit: %s with %s", from_pid, reason)
            return self._control_message(ctrl, None)

        elif m[0] == 'monitor_p':
            (_, src_pid, target_pid, ref) = m
            ctrl = (CONTROL_TERM_MONITOR_P, src_pid, target_pid, ref)
            # LOG.info("Monitor %s -> %s with %s", src_pid, target_pid, reason)
            return self._control_message(ctrl, None)

        elif m[0] in ['exit', 'exit2']:
            (_, from_pid, to_pid, reason) = m
            control_tag = CONTROL_TERM_EXIT if m[0] == 'exit' \
                else CONTROL_TERM_EXIT2
            ctrl = (control_tag, from_pid, to_pid, reason)
            LOG.info("Sending exit %s (node %s)", to_pid, to_pid.node_name_)
            return self._control_message(ctrl, None)

        elif m[0] == 'link':
            (_, pid1, pid2) = m
            ctrl = (CONTROL_TERM_LINK, pid1, pid2)
            LOG.info("Sending link %s <-> %s", pid1, pid2)
            return self._control_message(ctrl, None)

        LOG.error("Unhandled message to InConnection %s", m)

    @staticmethod
    def _control_term_send(from_pid, dst):
        """ Creates a control term for dist protocol.

            :type from_pid: term.pid.Pid
            :type dst: term.atom.Atom or term.pid.Pid
        """
        if isinstance(dst, Atom):
            return CONTROL_TERM_REG_SEND, from_pid, Atom(''), dst
        else:
            return CONTROL_TERM_SEND, Atom(''), dst

    def _control_message(self, ctrl, msg):
        """ Pack a control message and a regular message (can be None) together
            and send them over the connection
        """
        if msg is None:
            packet = b'p' + codec.term_to_binary(ctrl)
        else:
            packet = b'p' + codec.term_to_binary(ctrl) + codec.term_to_binary(
                msg)

        self._send_packet4(packet)

    @staticmethod
    def make_digest(challenge: int, cookie: str) -> bytes:
        result = md5(bytes(cookie, "ascii")
                     + bytes(str(challenge), "ascii")).digest()
        return result

    def protocol_error(self, msg) -> bool:
        LOG.error("Error: %s (state %s)" % (msg, self.state_))
        return False

    @staticmethod
    def check_digest(digest: bytes, challenge: int, cookie: str) -> bool:
        """ Hash cookie + the challenge together producing a verification hash
            and return if they match against the offered 'digest'.
        """
        expected_digest = BaseDistProtocol.make_digest(challenge, cookie)
        # LOG("Check digest: expected digest", expected_digest,
        #  "peer digest", digest)
        return digest == expected_digest

    async def on_packet_connected(self, data):
        """ Handle incoming dist packets in the connected state. """
        # TODO: Update timeout timer, that we have connectivity still
        if data == b'':
            self._send_packet4(b'')
            return True  # this was a keepalive

        msg_type = chr(data[0])

        if msg_type == "p":
            (control_term, tail) = codec.binary_to_term(data[1:])

            if tail != b'':
                (msg_term, tail) = codec.binary_to_term(tail)
            else:
                msg_term = None

            await self.on_passthrough_message(control_term, msg_term)

        else:
            return self.protocol_error(
                "Unexpected dist message type: %s" % msg_type)

        return True

    def report_dist_connected(self):
        assert (self.peer_name_ is not None)
        LOG.info("Connected to %s", self.peer_name_)
        self._inform_local_node(('node_connected', self.peer_name_, self))


__all__ = ['BaseDistProtocol']

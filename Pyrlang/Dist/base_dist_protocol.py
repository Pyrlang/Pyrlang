# Copyright 2018, Erlang Solutions Ltd.
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

import logging
import struct
from hashlib import md5
from typing import Union

from Term import util
from Pyrlang.Engine.base_engine import BaseEngine
from Pyrlang.Engine.base_protocol import BaseProtocol
from Term import codec
from Term.atom import Atom

LOG = logging.getLogger("Pyrlang.Dist")
LOG.setLevel(logging.INFO)

# Distribution protocol delivers pairs of (control_term, message).
# http://erlang.org/doc/apps/erts/erl_dist_protocol.html
# First element of control term in a 'p' message defines what it is
CONTROL_TERM_LINK = 1  # TODO
CONTROL_TERM_SEND = 2
CONTROL_TERM_EXIT = 3  # TODO
CONTROL_TERM_UNLINK = 4  # TODO
CONTROL_TERM_NODE_LINK = 5  # TODO
CONTROL_TERM_REG_SEND = 6
CONTROL_TERM_GROUP_LEADER = 7  # TODO
CONTROL_TERM_EXIT2 = 8  # TODO
CONTROL_TERM_SEND_TT = 12  # TODO
CONTROL_TERM_EXIT_TT = 13  # TODO
CONTROL_TERM_REG_SEND_TT = 16  # TODO
CONTROL_TERM_EXIT2_TT = 18  # TODO
CONTROL_TERM_MONITOR_P = 19
CONTROL_TERM_DEMONITOR_P = 20
CONTROL_TERM_MONITOR_P_EXIT = 21


class DistributionError(Exception):
    def __init__(self, msg, *args, **kwargs):
        LOG.error("DistributionError: %s", msg)
        Exception.__init__(self, msg, *args, **kwargs)


class BaseDistProtocol(BaseProtocol):
    """ Defines Erlang distribution protocol. """

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

    def __init__(self, node_name: str, engine: BaseEngine):
        """ Create connection handler object. """
        super().__init__()

        self.node_name_ = node_name
        """ Reference to the running Erlang node. (XXX forms a ref cycle) """

        self.packet_len_size_ = 2
        """ Packet size header is variable, 2 bytes before handshake is finished
            and 4 bytes afterwards. """

        self.addr_ = None

        self.engine_ = engine
        """ Save engine object, to use for our async needs later. """

        # refer to util.make_handler_in which reads this
        self.inbox_ = engine.queue_new()
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

        self._schedule_periodic_ping_remote()

    def on_connected(self, host_port):
        """ Handler invoked from the recv loop (in ``util.make_handler_in``)
            when the connection has been accepted and established.
        """
        self.addr_ = host_port
        self.state_ = self.RECV_NAME

    def on_incoming_data(self, data: bytes) -> Union[bytes, None]:
        if len(data) < self.packet_len_size_:
            # Not ready yet, keep reading
            return data

        # Dist protocol switches from 2 byte packet length to 4 at some point
        if self.packet_len_size_ == 2:
            pkt_size = util.u16(data, 0)
            offset = 2
        else:
            pkt_size = util.u32(data, 0)
            offset = 4

        if len(data) < self.packet_len_size_ + pkt_size:
            # Length is already visible but the data is not here yet
            return data

        packet = data[offset:(offset + pkt_size)]
        # LOG('Dist packet:', util.hex_bytes(packet))

        if self.on_packet(packet):
            return data[(offset + pkt_size):]

        # Protocol error has occured and instead we return None to request
        # connection close
        return None

    def on_packet(self, data) -> bool:
        pass

    def _get_node(self):
        from Pyrlang.node import Node
        return Node.all_nodes[self.node_name_]

    def on_connection_lost(self):
        """ Handler is called when the client has disconnected """
        self.state_ = self.DISCONNECTED

        if self.peer_name_ is not None:
            self._get_node().inbox_.put(("node_disconnected", self.peer_name_))

    def _send_packet2(self, content: bytes):
        """ Send a handshake-time status message with a 2 byte length prefix
        """
        # LOG.debug("pkt out %s", content)
        msg = struct.pack(">H", len(content)) + content
        self.send(msg)

    def _send_packet4(self, content: bytes):
        """ Send a connection-time status message with a 4 byte length prefix
        """
        # if content != b'':
        #     LOG.debug("pkt out %s", content)
        msg = struct.pack(">I", len(content)) + content
        self.send(msg)

    def on_passthrough_message(self, control_term, msg_term):
        """ On incoming 'p' message with control and data, handle it.
            :raises DistributionError: when 'p' message is not a tuple
        """
        # LOG.debug("Passthrough msg term=%s; control_term=%s",
        #           msg_term, control_term)

        if type(control_term) != tuple:
            raise DistributionError("In a 'p' message control term must be a "
                                    "tuple")

        ctrl_msg_type = control_term[0]

        n = self._get_node()

        if ctrl_msg_type == CONTROL_TERM_REG_SEND:
            return n.send(sender=control_term[1],
                          receiver=control_term[3],
                          message=msg_term)

        elif ctrl_msg_type == CONTROL_TERM_SEND:
            return n.send(sender=None,
                          receiver=control_term[2],
                          message=msg_term)

        elif ctrl_msg_type == CONTROL_TERM_MONITOR_P:
            (_, sender, target, ref) = control_term
            from Pyrlang.node import ProcessNotFoundError
            try:
                return n.monitor_process(origin=sender,
                                         target=target)
            except ProcessNotFoundError:
                pass

        elif ctrl_msg_type == CONTROL_TERM_DEMONITOR_P:
            (_, sender, target, ref) = control_term
            from Pyrlang.node import ProcessNotFoundError
            try:
                return n.demonitor_process(origin=sender,
                                           target=target)
            except ProcessNotFoundError:
                pass

        else:
            LOG.error("Unhandled 'p' message: %s; %s", control_term, msg_term)

    def periodic_check(self):
        while True:
            msg = self.inbox_.get()
            if msg is None:
                break
            self._handle_one_inbox_message(msg)

    def _periodic_ping_remote(self):
        self._schedule_periodic_ping_remote()

        if self.state_ == self.CONNECTED and self.packet_len_size_ == 4:
            self._send_packet4(b'')

    def _schedule_periodic_ping_remote(self):
        self.engine_.call_later(15.0, self._periodic_ping_remote)

    def _handle_one_inbox_message(self, m):
        # Send a ('send', Dst, Msg) to deliver a message to the other side
        if m[0] == 'send':
            (_, from_pid, dst, msg) = m
            ctrl = self._control_term_send(from_pid=from_pid, dst=dst)
            LOG.info("Control msg %s; %s" % (ctrl, msg))
            return self._control_message(ctrl, msg)

        elif m[0] == 'monitor_p_exit':
            (_, from_pid, to_pid, ref, reason) = m
            ctrl = (CONTROL_TERM_MONITOR_P_EXIT,
                    from_pid, to_pid, ref, reason)
            LOG.info("Monitor proc exit: %s with %s", from_pid, reason)
            return self._control_message(ctrl, None)

        LOG.error("Unhandled message to InConnection %s" % m)

    @staticmethod
    def _control_term_send(from_pid, dst):
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
            packet = b'p' + codec.term_to_binary(ctrl) + codec.term_to_binary(msg)

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

    def on_packet_connected(self, data):
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

            self.on_passthrough_message(control_term, msg_term)

        else:
            return self.protocol_error(
                "Unexpected dist message type: %s" % msg_type)

        return True

    def report_dist_connected(self):
        assert (self.peer_name_ is not None)
        LOG.info("Connected to %s", self.peer_name_)
        self._get_node().inbox_.put(('node_connected', self.peer_name_, self))


__all__ = ['BaseDistProtocol', 'DistributionError']

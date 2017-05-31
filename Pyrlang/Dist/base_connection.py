# Copyright 2017, Erlang Solutions Ltd.
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

from __future__ import print_function

import struct
from abc import abstractmethod
from typing import Union

from _md5 import md5
from gevent.queue import Queue

from Pyrlang import logger
from Pyrlang.Dist import util, etf
from Pyrlang.Dist.node_opts import NodeOpts

LOG = logger.nothing
ERROR = logger.tty

# First element of control term in a 'p' message defines what it is
CONTROL_TERM_SEND = 2
CONTROL_TERM_REG_SEND = 6
CONTROL_TERM_MONITOR_P = 19
CONTROL_TERM_DEMONITOR_P = 20
CONTROL_TERM_MONITOR_P_EXIT = 21


class BaseConnection:
    DISCONNECTED = 0
    RECV_NAME = 1
    WAIT_CHALLENGE_REPLY = 2
    CONNECTED = 3

    def __init__(self, dist, node_opts: NodeOpts):
        self.state_ = self.DISCONNECTED
        self.packet_len_size_ = 2
        """ Packet size header is variable, 2 bytes before handshake is finished
            and 4 bytes afterwards. """

        self.socket_ = None
        self.addr_ = None

        # TODO: Pass as argument and save node instead of dist
        self.dist_ = dist  # reference to distribution object

        self.node_opts_ = node_opts
        self.inbox_ = Queue()  # refer to util.make_handler_in which reads this
        """ Inbox is used to ask the connection to do something. """

        self.peer_distr_version_ = (None, None)
        """ Protocol version range supported by the remote peer. Erlang/OTP 
            versions 19-20 supports protocol version 7, older Erlangs down to 
            R6B support version 5. """

        self.peer_flags_ = 0
        self.peer_name_ = None
        self.my_challenge_ = None

    def on_connected(self, sockt, address):
        """ Handler invoked from the recv loop (in ``util.make_handler_in``)
            when the connection has been accepted and established.
        """
        self.state_ = self.RECV_NAME
        self.socket_ = sockt
        self.addr_ = address

    def consume(self, data: bytes) -> Union[bytes, None]:
        """ Attempt to consume first part of data as a packet

            :param data: The accumulated data from the socket which we try to
                partially or fully consume
            :return: Unconsumed data, incomplete following packet maybe or
                nothing. Returning None requests to close the connection
        """
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
        # connection close (see util.py)
        return None

    @abstractmethod
    def on_packet(self, data) -> bool:
        pass

    def on_connection_lost(self):
        """ Handler is called when the client has disconnected
        """
        self.state_ = self.DISCONNECTED

        from Pyrlang.node import Node
        Node.singleton.inbox_.put(
            ('node_disconnected', self.peer_name_))

    def _send_packet2(self, content: bytes):
        """ Send a handshake-time status message with a 2 byte length prefix
        """
        msg = struct.pack(">H", len(content)) + content
        self.socket_.sendall(msg)

    def _send_packet4(self, content: bytes):
        """ Send a connection-time status message with a 4 byte length prefix
        """
        msg = struct.pack(">I", len(content)) + content
        self.socket_.sendall(msg)

    @staticmethod
    def on_passthrough_message(control_term, msg_term):
        """ On incoming 'p' message with control and data, handle it.
            :raises DistributionError: when 'p' message is not a tuple
        """
        LOG("Passthrough msg %s\n%s" % (control_term, msg_term))

        if type(control_term) != tuple:
            raise DistributionError("In a 'p' message control term must be a "
                                    "tuple")

        ctrl_msg_type = control_term[0]

        from Pyrlang import node
        the_node = node.Node.singleton

        if ctrl_msg_type in [CONTROL_TERM_SEND, CONTROL_TERM_REG_SEND]:
            return the_node.send(sender=control_term[1],
                                 receiver=control_term[3],
                                 message=msg_term)

        elif ctrl_msg_type == CONTROL_TERM_MONITOR_P:
            (_, sender, target, ref) = control_term
            return the_node.monitor_process(origin=sender,
                                            target=target)

        elif ctrl_msg_type == CONTROL_TERM_DEMONITOR_P:
            (_, sender, target, ref) = control_term
            return the_node.demonitor_process(origin=sender,
                                              target=target)

        else:
            ERROR("Unhandled 'p' message: %s\n%s" % (control_term, msg_term))

    def handle_one_inbox_message(self, m):
        # Send a ('send', Dst, Msg) to deliver a message to the other side
        if m[0] == 'send':
            (_, dst, msg) = m
            ctrl = self._control_term_send(dst)
            LOG("Connection: control msg %s; %s" % (ctrl, msg))
            return self._control_message(ctrl, msg)

        elif m[0] == 'monitor_p_exit':
            (_, from_pid, to_pid, ref, reason) = m
            ctrl = (CONTROL_TERM_MONITOR_P_EXIT,
                    from_pid, to_pid, ref, reason)
            LOG("Monitor proc exit: %s with %s" % (from_pid, reason))
            return self._control_message(ctrl, None)

        ERROR("Connection: Unhandled message to InConnection %s" % m)

    @staticmethod
    def _control_term_send(dst):
        return CONTROL_TERM_SEND, term.Atom(''), dst

    def _control_message(self, ctrl, msg):
        """ Pack a control message and a regular message (can be None) together
            and send them over the connection
        """
        if msg is None:
            packet = b'p' + etf.term_to_binary(ctrl)
        else:
            packet = b'p' + etf.term_to_binary(ctrl) + etf.term_to_binary(msg)

        self._send_packet4(packet)

    @staticmethod
    def make_digest(challenge: int, cookie: str) -> bytes:
        result = md5(bytes(cookie, "ascii")
                     + bytes(str(challenge), "ascii")).digest()
        return result


def protocol_error(msg) -> False:
    ERROR("Distribution protocol error:", msg)
    return False


__all__ = ['protocol_error', 'BaseConnection']

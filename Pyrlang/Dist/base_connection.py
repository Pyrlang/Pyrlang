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
from abc import abstractmethod
from typing import Union

from gevent.queue import Queue

from Pyrlang import logger
from Pyrlang.Dist import util
from Pyrlang.Dist.node_opts import NodeOpts

LOG = logger.nothing
ERROR = logger.tty


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


def protocol_error(msg) -> False:
    ERROR("Distribution protocol error:", msg)
    return False


__all__ = ['protocol_error', 'BaseConnection']

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

""" The module implements outgoing TCP distribution connection (i.e. initiated
    by our node to another node with the help of EPMD).
"""

from __future__ import print_function

import random

from Pyrlang import logger
from Pyrlang.Dist import util, dist_protocol
from Pyrlang.Dist.base_connection import *

LOG = logger.tty
WARN = logger.tty
ERROR = logger.tty


class OutConnection(BaseConnection):
    """ Handles outgoing connections from our to other nodes.

        Behaves like a ``Greenlet`` but the actual recv loop around this
        protocol is located in the ``util.connect_with`` helper function.
    """
    DISCONNECTED = 'disconn'

    CONNECTED = 'conn'

    RECV_STATUS = 'recv_status'

    # State 'alive' means that this connection is duplicate, next message
    # may be 'true' to allow using this new connection or 'false', requesting
    # this connection to be closed
    ALIVE = 'alive'

    RECV_CHALLENGE = 'recv_challenge'
    RECV_CHALLENGE_ACK = 'recv_challenge_ack'

    def __init__(self, node):
        BaseConnection.__init__(self, node)
        self.state_ = self.DISCONNECTED

    def on_connected(self, sockt, address):
        BaseConnection.on_connected(self, sockt=sockt, address=address)
        self._send_name()
        self.state_ = self.RECV_STATUS

    def on_connection_lost(self):
        BaseConnection.on_connection_lost(self)
        self.state_ = self.DISCONNECTED

    def on_packet(self, data) -> bool:
        """ Handle incoming distribution packet

            :param data: The packet after the header had been removed
        """
        # LOG("Dist-out[%s]: recv %s" % (self.state_, data))

        if self.state_ == self.CONNECTED:
            return self.on_packet_connected(data)

        if self.state_ == self.RECV_STATUS:
            return self.on_packet_recvstatus(data)

        elif self.state_ == self.RECV_CHALLENGE:
            return self.on_packet_recvchallenge(data)

        elif self.state_ == self.RECV_CHALLENGE_ACK:
            return self.on_packet_recvchallenge_ack(data)

        elif self.state_ == self.ALIVE:
            return self.on_packet_alive(data)

        raise DistributionError("Unknown state for on_packet: %s" % self.state_)

    def _send_name(self):
        """ Create and send first welcome packet. """
        pkt = b'n' + \
              bytes([dist_protocol.DIST_VSN, dist_protocol.DIST_VSN]) + \
              util.to_u32(self.node_.node_opts_.dflags_) + \
              bytes(str(self.node_.name_), "latin-1")
        LOG("Dist-out: Send_name", pkt)
        self._send_packet2(pkt)

    def on_packet_recvstatus(self, data):
        if chr(data[0]) != 's':
            return self.protocol_error("Handshake 's' packet expected")

        if data == b'salive':  # a duplicate connection detected
            self.state_ = self.ALIVE
            return True

        if data != b'sok' and data != b'ok_simultaneous':
            return self.protocol_error("Handshake bad status: %s" % data)

        self.state_ = self.RECV_CHALLENGE
        return True

    def on_packet_alive(self, data):
        if data == b'true':
            self.state_ = self.CONNECTED  # not sure if we have to challenge
            return True

        return self.protocol_error("Duplicate connection denied")

    def on_packet_recvchallenge(self, data):
        if chr(data[0]) != 'n':
            return self.protocol_error("Handshake 'n' packet expected")

        self.peer_distr_version_ = (data[1], data[2])
        self.peer_flags_ = util.u32(data, 3)
        challenge = util.u32(data, 7)
        self.peer_name_ = data[11:].decode("latin1")

        self._send_challenge_reply(challenge)

        self.state_ = self.RECV_CHALLENGE_ACK
        return True

    def _send_challenge_reply(self, challenge: int):
        digest = self.make_digest(challenge, self.node_.get_cookie())
        self.my_challenge_ = int(random.random() * 0x7fffffff)

        pkt = b'r' + util.to_u32(self.my_challenge_) + digest
        return self._send_packet2(pkt)

    def on_packet_recvchallenge_ack(self, data):
        if chr(data[0]) != 'a':
            return self.protocol_error("Handshake 'r' packet expected")

        digest = data[1:]
        if not self.check_digest(digest=digest,
                                 challenge=self.my_challenge_,
                                 cookie=self.node_.get_cookie()):
            return self.protocol_error("Handshake digest verification failed")

        self.packet_len_size_ = 4
        self.state_ = self.CONNECTED
        self.report_dist_connected()

        # TODO: start timer with node_opts_.network_tick_time_

        LOG("Out-connection established with %s" % self.peer_name_)
        return True

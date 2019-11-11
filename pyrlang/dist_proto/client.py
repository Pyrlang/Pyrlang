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

""" The module implements outgoing TCP dist_proto connection (i.e. initiated
    by our node to another node with the help of EPMD).
"""
import asyncio
import logging
import random

from pyrlang.dist_proto import version
from pyrlang.dist_proto.base_dist_protocol import BaseDistProtocol
from term import util

LOG = logging.getLogger(__name__)
LOG.setLevel(logging.INFO)


class DistClientProtocol(BaseDistProtocol):
    """ Handles outgoing connections from our to other nodes. """

    def __init__(self, node_name: str):
        super().__init__(node_name=node_name)

    def connection_made(self, transport: asyncio.Transport):
        super().connection_made(transport)
        self._send_name()
        self.state_ = self.RECV_STATUS

    def on_packet(self, data: bytes) -> bytes:
        """ Handle incoming dist_proto packet

            :param data: The packet after the header had been removed
        """

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

        self.protocol_error("Unknown state for on_packet: %s" % self.state_)

    def _send_name(self):
        """ Create and send first welcome packet. """
        pkt = b'n' + \
              bytes([version.DIST_VSN, version.DIST_VSN]) + \
              util.to_u32(self.get_node().node_opts_.dflags_) + \
              bytes(self.node_name_, "latin-1")
        LOG.info("send_name %s (name=%s)", pkt, self.node_name_)
        self._send_packet2(pkt)

    def on_packet_recvstatus(self, data: bytes) -> bytes:
        if chr(data[0]) != 's':
            self.protocol_error("Handshake 's' packet expected")
            # raise

        # a duplicate connection detected
        if data.startswith(b'salive'):
            self.state_ = self.ALIVE
            return data[6:]  # cut after b'salive'

        if data.startswith(b'sok_simultaneous'):
            self.state_ = self.RECV_CHALLENGE
            return data[16:]  # cut after b'sok_simultaneous'

        if data.startswith(b'sok'):
            self.state_ = self.RECV_CHALLENGE
            return data[3:]  # cut after b'sok'

        self.protocol_error("Handshake bad status: %s" % data)
        # raise

    def on_packet_alive(self, data: bytes) -> bytes:
        if data.startswith(b'true'):
            self.state_ = self.CONNECTED  # not sure if we have to challenge
            return data[4:]

        self.protocol_error("Duplicate connection denied")
        # raise

    def on_packet_recvchallenge(self, data: bytes) -> bytes:
        if chr(data[0]) != 'n':
            return self.protocol_error("Handshake 'n' packet expected")

        self.peer_distr_version_ = (data[1], data[2])
        self.peer_flags_ = util.u32(data, 3)
        challenge = util.u32(data, 7)
        self.peer_name_ = data[11:].decode("latin1")

        self._send_challenge_reply(challenge)

        self.state_ = self.RECV_CHALLENGE_ACK
        return b''  # assume everything is consumed?

    def _send_challenge_reply(self, challenge: int):
        digest = self.make_digest(challenge, self.get_node().get_cookie())
        self.my_challenge_ = int(random.random() * 0x7fffffff)

        pkt = b'r' + util.to_u32(self.my_challenge_) + digest
        return self._send_packet2(pkt)

    def on_packet_recvchallenge_ack(self, data: bytes) -> bytes:
        if chr(data[0]) != 'a':
            self.protocol_error("Handshake 'r' packet expected")
            # raise

        digest = data[1:]
        if not self.check_digest(digest=digest,
                                 challenge=self.my_challenge_,
                                 cookie=self.get_node().get_cookie()):
            self.protocol_error("Handshake digest verification failed")
            # raise

        self.packet_len_size_ = 4
        self.state_ = self.CONNECTED
        self.report_dist_connected()

        # TODO: start timer with node_opts_.network_tick_time_

        LOG.info("Outgoing dist connection: established with %s",
                 self.peer_name_)
        return b''  # assume everything is consumed?

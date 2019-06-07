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

from pyrlang2.dist_proto import version
from pyrlang2.dist_proto.base_dist_protocol import BaseDistProtocol, DistributionError
from term import util

LOG = logging.getLogger("pyrlang.dist")
LOG.setLevel(logging.INFO)


class DistClientProtocol(BaseDistProtocol):
    """ Handles outgoing connections from our to other nodes. """

    def __init__(self, node_name: str):
        super().__init__(node_name=node_name)

    def connection_made(self, transport: asyncio.Transport):
        super().connection_made(transport)
        self._send_name()
        self.state_ = self.RECV_STATUS

    def on_packet(self, data) -> bool:
        """ Handle incoming dist_proto packet

            :param data: The packet after the header had been removed
        """
        # LOG("Dist-out[%s]: recv %s" % (self.state_, data))

        if self.state_ == self.CONNECTED:
            asyncio.get_event_loop().create_task(self.on_packet_connected(data))
            return True

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
              bytes([version.DIST_VSN, version.DIST_VSN]) + \
              util.to_u32(self.get_node().node_opts_.dflags_) + \
              bytes(self.node_name_, "latin-1")
        LOG.info("send_name %s (name=%s)", pkt, self.node_name_)
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
        digest = self.make_digest(challenge, self.get_node().get_cookie())
        self.my_challenge_ = int(random.random() * 0x7fffffff)

        pkt = b'r' + util.to_u32(self.my_challenge_) + digest
        return self._send_packet2(pkt)

    def on_packet_recvchallenge_ack(self, data):
        if chr(data[0]) != 'a':
            return self.protocol_error("Handshake 'r' packet expected")

        digest = data[1:]
        if not self.check_digest(digest=digest,
                                 challenge=self.my_challenge_,
                                 cookie=self.get_node().get_cookie()):
            return self.protocol_error("Handshake digest verification failed")

        self.packet_len_size_ = 4
        self.state_ = self.CONNECTED
        self.report_dist_connected()

        # TODO: start timer with node_opts_.network_tick_time_

        LOG.info("Outgoing dist connection: established with %s" % self.peer_name_)
        return True

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

""" The module implements incoming TCP dist_proto protocol (i.e. initiated
    by another node with the help of EPMD). Protocol only performs handling of
    incoming data, the socket is handled by the Async Engine (pyrlang.async).
"""

import logging
import random
import struct

from pyrlang.dist_proto import version
from pyrlang.dist_proto.base_dist_protocol import BaseDistProtocol, \
    DistributionError
from term import util

LOG = logging.getLogger(__name__)
# LOG.setLevel(logging.INFO)


class DistServerProtocol(BaseDistProtocol):
    """ Protocol handles incoming connections from other nodes.
    """

    def __init__(self, node_name: str):
        super().__init__(node_name=node_name)

    def connection_lost(self, exc):
        super().connection_lost(exc)
        self.state_ = self.DISCONNECTED

    def on_packet(self, data: bytes) -> bytes:
        """ Handle incoming dist_proto packet

            :param data: The packet after the header had been removed
        """
        if self.state_ == self.CONNECTED:  # this goes first for perf reasons
            return self.on_packet_connected(data)

        elif self.state_ == self.RECV_NAME:
            return self.on_packet_recvname(data)

        elif self.state_ == self.WAIT_CHALLENGE_REPLY:
            return self.on_packet_challengereply(data)

        raise DistributionError("Unknown state for on_packet: %s" % self.state_)

    def on_packet_recvname(self, data: bytes) -> bytes:
        """ Handle RECV_NAME command, the first packet in a new connection. """
        if not data.startswith(b'n'):
            self.protocol_error("Unexpected packet (expecting RECV_NAME)")
            # raise

        # Read peer dist_proto version and compare to ours
        peer_max_min = (data[1], data[2])
        #if version.dist_version_check(peer_max_min):
        if not version.check_valid_dist_version(peer_max_min):
            self.protocol_error(
                "Dist protocol version have: %s got: %s"
                % (str(version.DIST_VSN_PAIR), str(peer_max_min))
            )
            # raise

        self.peer_distr_version_ = peer_max_min
        self.peer_flags_ = util.u32(data[3:7])
        self.peer_name_ = data[7:].decode("latin1")
        LOG.info("RECV_NAME: %s %s", self.peer_distr_version_, self.peer_name_)

        # Report
        self._send_packet2(b'sok')

        self.my_challenge_ = int(random.random() * 0x7fffffff)
        self._send_challenge(self.my_challenge_)

        self.state_ = self.WAIT_CHALLENGE_REPLY

        return b''  # assume everything is consumed

    def on_packet_challengereply(self, data: bytes) -> bytes:
        if not data.startswith(b'r'):
            self.protocol_error(
                "Unexpected packet (expecting CHALLENGE_REPLY) %s" % data
            )
            # raise

        peers_challenge = util.u32(data, 1)
        peer_digest = data[5:]
        LOG.info("challengereply: peer's challenge %s", peers_challenge)

        my_cookie = self.get_node().node_opts_.cookie_
        if not self.check_digest(digest=peer_digest,
                                 challenge=self.my_challenge_,
                                 cookie=my_cookie):
            self.protocol_error("Disallowed node connection (check the cookie)")
            # raise

        self._send_challenge_ack(peers_challenge, my_cookie)
        self.packet_len_size_ = 4
        self.state_ = self.CONNECTED
        self.report_dist_connected()

        # TODO: start timer with node_opts_.network_tick_time_

        LOG.info("Incoming dist established from %s", self.peer_name_)
        return b''  # assume data is consumed

    def _send_challenge(self, my_challenge):
        n = self.get_node()
        LOG.info("Sending challenge (our number is %d) %s"
                 % (my_challenge, self.node_name_))
        msg = b'n' \
              + struct.pack(">HII",
                            version.DIST_VSN,
                            n.node_opts_.dflags_,
                            my_challenge) \
              + bytes(self.node_name_, "latin1")
        self._send_packet2(msg)

    def _send_challenge_ack(self, peers_challenge: int, cookie: str):
        """ After cookie has been verified, send the confirmation by digesting
            our cookie with the remote challenge
        """
        digest = DistServerProtocol.make_digest(peers_challenge, cookie)
        self._send_packet2(b'a' + digest)


__all__ = ['DistServerProtocol']

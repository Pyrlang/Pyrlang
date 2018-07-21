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

""" The module implements incoming TCP distribution connection (i.e. initiated
    by another node with the help of EPMD).
"""

import random
import logging
import struct

from Pyrlang.Dist import util, dist_protocol
from Pyrlang.Dist.base_connection import *


class InConnection(BaseConnection):
    """ Handles incoming connections from other nodes.

        Behaves like a ``Greenlet`` but the actual Greenlet run procedure and
        the recv loop around this protocol are located in the
        ``util.make_handler_in`` helper function.
    """

    DISCONNECTED = 'disconn'
    CONNECTED = 'conn'
    RECV_NAME = 'recvname'
    WAIT_CHALLENGE_REPLY = 'wait_ch_reply'

    def __init__(self, node):
        BaseConnection.__init__(self, node)
        self.state_ = self.DISCONNECTED

    def on_connected(self, sockt, address):
        BaseConnection.on_connected(self, sockt=sockt, address=address)
        self.state_ = self.RECV_NAME

    def on_connection_lost(self):
        BaseConnection.on_connection_lost(self)
        self.state_ = self.DISCONNECTED

    def on_packet(self, data) -> bool:
        """ Handle incoming distribution packet

            :param data: The packet after the header had been removed
        """
        if self.state_ == self.CONNECTED:  # this goes first for perf reasons
            return self.on_packet_connected(data)

        elif self.state_ == self.RECV_NAME:
            return self.on_packet_recvname(data)

        elif self.state_ == self.WAIT_CHALLENGE_REPLY:
            return self.on_packet_challengereply(data)

        raise DistributionError("Unknown state for on_packet: %s" % self.state_)

    def on_packet_recvname(self, data) -> bool:
        """ Handle RECV_NAME command, the first packet in a new connection. """
        if data[0] != ord('n'):
            return self.protocol_error(
                "Unexpected packet (expecting RECV_NAME)")

        # Read peer distribution version and compare to ours
        peer_max_min = (data[1], data[2])
        if dist_protocol.dist_version_check(peer_max_min):
            return self.protocol_error(
                "Dist protocol version have: %s got: %s"
                % (str(dist_protocol.DIST_VSN_PAIR), str(peer_max_min)))
        self.peer_distr_version_ = peer_max_min

        self.peer_flags_ = util.u32(data[3:7])
        self.peer_name_ = data[7:].decode("latin1")
        logging.info("RECV_NAME:", self.peer_distr_version_, self.peer_name_)

        # Report
        self._send_packet2(b"sok")

        self.my_challenge_ = int(random.random() * 0x7fffffff)
        self._send_challenge(self.my_challenge_)

        self.state_ = self.WAIT_CHALLENGE_REPLY

        return True

    def on_packet_challengereply(self, data):
        if data[0] != ord('r'):
            return self.protocol_error(
                "Unexpected packet (expecting CHALLENGE_REPLY)")

        peers_challenge = util.u32(data, 1)
        peer_digest = data[5:]
        logging.info("challengereply: peer's challenge", peers_challenge)

        my_cookie = self.node_.node_opts_.cookie_
        if not self.check_digest(digest=peer_digest,
                                 challenge=self.my_challenge_,
                                 cookie=my_cookie):
            return self.protocol_error(
                "Disallowed node connection (check the cookie)")

        self._send_challenge_ack(peers_challenge, my_cookie)
        self.packet_len_size_ = 4
        self.state_ = self.CONNECTED
        self.report_dist_connected()

        # TODO: start timer with node_opts_.network_tick_time_

        logging.info("In-connection established with %s" % self.peer_name_)
        return True

    def _send_challenge(self, my_challenge):
        logging.info("Sending challenge (our number is %d)" % my_challenge,
                     self.node_.dist_.name_)
        msg = b'n' \
              + struct.pack(">HII",
                            dist_protocol.DIST_VSN,
                            self.node_.node_opts_.dflags_,
                            my_challenge) \
              + bytes(self.node_.dist_.name_, "latin1")
        self._send_packet2(msg)

    def _send_challenge_ack(self, peers_challenge: int, cookie: str):
        """ After cookie has been verified, send the confirmation by digesting
            our cookie with the remote challenge
        """
        digest = InConnection.make_digest(peers_challenge, cookie)
        self._send_packet2(b'a' + digest)


__all__ = ['InConnection']

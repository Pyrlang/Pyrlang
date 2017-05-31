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

""" The module implements incoming TCP distribution connection (i.e. initiated
    by another node with the help of EPMD).
"""
from __future__ import print_function

import random
import struct

from Pyrlang import logger
from Pyrlang.Dist import util, etf, dist_protocol
from Pyrlang.Dist.base_connection import BaseConnection, protocol_error
from Pyrlang.Dist.node_opts import NodeOpts

LOG = logger.nothing
ERROR = logger.tty


class DistributionError(Exception):
    pass


class InConnection(BaseConnection):
    """ Handles incoming connections from other nodes.

        Behaves like a ``Greenlet`` but the actual Greenlet run procedure and
        the recv loop around this protocol are located in the
        ``util.make_handler_in`` helper function.
    """

    def __init__(self, dist, node_opts: NodeOpts):
        BaseConnection.__init__(self, dist, node_opts)

    def on_packet(self, data) -> bool:
        """ Handle incoming distribution packet

            :param data: The packet after the header had been removed
        """
        if self.state_ == self.RECV_NAME:
            return self.on_packet_recvname(data)

        elif self.state_ == self.WAIT_CHALLENGE_REPLY:
            return self.on_packet_challengereply(data)

        elif self.state_ == self.CONNECTED:
            return self.on_packet_connected(data)

    def on_packet_recvname(self, data) -> bool:
        """ Handle RECV_NAME command, the first packet in a new connection. """
        if data[0] != ord('n'):
            return protocol_error("Unexpected packet (expecting RECV_NAME)")

        # Read peer distribution version and compare to ours
        peer_max_min = (data[1], data[2])
        if dist_protocol.dist_version_check(peer_max_min):
            return protocol_error(
                "Dist protocol version have: %s got: %s"
                % (str(dist_protocol.DIST_VSN_PAIR), str(peer_max_min)))
        self.peer_distr_version_ = peer_max_min

        self.peer_flags_ = util.u32(data[3:7])
        self.peer_name_ = data[7:].decode("latin1")
        LOG("RECV_NAME:", self.peer_distr_version_, self.peer_name_)

        # Maybe too early here? Actual connection is established moments later
        from Pyrlang.node import Node
        Node.singleton.inbox_.put(
            ('node_connected', self.peer_name_, self))

        # Report
        self._send_packet2(b"sok")

        self.my_challenge_ = int(random.random() * 0x7fffffff)
        self._send_challenge(self.my_challenge_)

        self.state_ = self.WAIT_CHALLENGE_REPLY

        return True

    def on_packet_challengereply(self, data):
        if data[0] != ord('r'):
            return protocol_error(
                "Unexpected packet (expecting CHALLENGE_REPLY)")

        peers_challenge = util.u32(data, 1)
        peer_digest = data[5:]
        LOG("challengereply: peer's challenge", peers_challenge)

        my_cookie = self.node_opts_.cookie_
        if not self._check_digest(peer_digest, self.my_challenge_, my_cookie):
            return protocol_error(
                "Disallowed node connection (check the cookie)")

        self._send_challenge_ack(peers_challenge, my_cookie)
        self.packet_len_size_ = 4
        self.state_ = self.CONNECTED

        # TODO: start timer with node_opts_.network_tick_time_

        LOG("Connection established with %s" % self.peer_name_)
        return True

    def on_packet_connected(self, data):
        # TODO: Update timeout timer, that we have connectivity still
        if data == b'':
            self._send_packet4(b'')
            return True  # this was a keepalive

        msg_type = chr(data[0])

        if msg_type == "p":
            (control_term, tail) = etf.binary_to_term(data[1:])

            if tail != b'':
                (msg_term, tail) = etf.binary_to_term(tail)
            else:
                msg_term = None
            self.on_passthrough_message(control_term, msg_term)

        else:
            return protocol_error("Unexpected dist message type: %s" % msg_type)

        return True

    def _send_challenge(self, my_challenge):
        LOG("Sending challenge (our number is %d)" % my_challenge,
            self.dist_.name_)
        msg = b'n' \
              + struct.pack(">HII",
                            dist_protocol.DIST_VSN,
                            self.node_opts_.dflags_,
                            my_challenge) \
              + bytes(self.dist_.name_, "latin1")
        self._send_packet2(msg)

    @staticmethod
    def _check_digest(peer_digest: bytes, peer_challenge: int,
                      cookie: str) -> bool:
        """ Hash cookie + the challenge together producing a verification hash
        """
        expected_digest = InConnection.make_digest(peer_challenge, cookie)
        LOG("Check digest: expected digest", expected_digest,
            "peer digest", peer_digest)
        return peer_digest == expected_digest

    def _send_challenge_ack(self, peers_challenge: int, cookie: str):
        """ After cookie has been verified, send the confirmation by digesting
            our cookie with the remote challenge
        """
        digest = InConnection.make_digest(peers_challenge, cookie)
        self._send_packet2(b'a' + digest)


__all__ = ['InConnection']

import random
import struct
from hashlib import md5
from typing import Union

from Pyrlang.Dist import epmd, util
from Pyrlang.Dist.node_opts import ErlNodeOpts


class InConnection:
    """ Handling incoming connections from other nodes.
        Called from handler provided by util.make_handler
    """
    DISCONNECTED = 0
    RECV_NAME = 1
    WAIT_CHALLENGE_REPLY = 2
    CONNECTED = 3

    def __init__(self, dist, node_opts: ErlNodeOpts):
        self.state_ = self.DISCONNECTED
        self.packet_len_size_ = 2
        self.socket_ = None
        self.addr_ = None

        self.dist_ = dist  # reference to distribution object
        self.node_opts_ = node_opts

        self.peer_distr_version_ = (None, None)
        self.peer_flags_ = 0
        self.peer_name_ = None
        self.my_challenge_ = None

    def on_connected(self, sockt, address):
        self.state_ = self.RECV_NAME
        self.socket_ = sockt
        self.addr_ = address

    def consume(self, data: bytes) -> Union[bytes, None]:
        """ Attempt to consume first part of data as a packet
        :param data: The accumulated data from the socket which we try to
            partially or fully consume
        :return: Unconsumed data, incomplete following packet maybe or nothing
            Returning None requests to close the connection
        """
        if len(data) < self.packet_len_size_:
            # Not ready yet, keep reading
            return data

        # Dist protocol switches from 2 byte packet length to 4 at some point
        if self.packet_len_size_ == 2:
            pkt_size = self._as_u16(data, 0)
            offset = 2
        else:
            pkt_size = self._as_u32(data, 0)
            offset = 4

        if len(data) < self.packet_len_size_ + pkt_size:
            # Length is already visible but the data is not here yet
            return data

        packet = data[offset:(offset + pkt_size)]
        print('Dist packet:', util.hex_bytes(packet))

        if self.on_packet(packet):
            return data[(offset + pkt_size):]

        # Protocol error has occured and instead we return None to request
        # connection close (see util.py)
        return None

    def on_connection_lost(self):
        """ Handler is called when the client has disconnected
        """
        self.state_ = self.DISCONNECTED

    def on_packet(self, data) -> bool:
        """ Handle incoming distribution packet
        :param data: The packet after the header with length has been removed
        """
        if self.state_ == self.RECV_NAME:
            return self.on_packet_recvname(data)
        elif self.state_ == self.WAIT_CHALLENGE_REPLY:
            return self.on_packet_challengereply(data)
        elif self.state_ == self.CONNECTED:
            return self.on_packet_connected(data)

    @staticmethod
    def _as_u16(data: bytes, pos: int = 0):
        return struct.unpack(">H", data[pos:pos+2])[0]

    @staticmethod
    def _as_u32(data: bytes, pos: int = 0):
        return struct.unpack(">I", data[pos:pos+4])[0]

    @staticmethod
    def error(msg) -> False:
        print("Distribution protocol error:", msg)
        return False

    @staticmethod
    def _dist_version_check(pdv: tuple):
        """ Check peer version against our version
            :type pdv: (MAX,MIN) - peer dist version, supported by the peer
        """
        return pdv[0] >= epmd.DIST_VSN >= pdv[1]

    def on_packet_recvname(self, data) -> bool:
        if data[0] != ord('n'):
            return self.error("Unexpected packet (expecting RECV_NAME)")

        # Read peer distribution version and compare to ours
        pdv = (data[1], data[2])
        if self._dist_version_check(pdv):
            return self.error("Dist protocol version have: %s got: %s"
                              % (str(epmd.DIST_VSN_PAIR), str(pdv)))
        self.peer_distr_version_ = pdv

        self.peer_flags_ = self._as_u32(data[3:7])
        self.peer_name_ = data[7:].decode("latin1")
        print("RECV_NAME:", self.peer_distr_version_, self.peer_name_)

        # Report
        self._send_packet2(b"sok")

        self.my_challenge_ = int(random.random() * 0x7fffffff)
        self._send_challenge(self.my_challenge_)

        self.state_ = self.WAIT_CHALLENGE_REPLY

        return True

    def on_packet_challengereply(self, data):
        if data[0] != ord('r'):
            return self.error("Unexpected packet (expecting CHALLENGE_REPLY)")

        peers_challenge = self._as_u32(data, 1)
        peer_digest = data[5:]
        print("challengereply: peer's challenge", peers_challenge)

        my_cookie = self.node_opts_.cookie_
        if not self._check_digest(peer_digest, self.my_challenge_, my_cookie):
            return self.error("Disallowed node connection (check the cookie)")

        self._send_challenge_ack(peers_challenge, my_cookie)
        self.packet_len_size_ = 4
        self.state_ = self.CONNECTED
        # TODO: start timer with node_opts_.network_tick_time_

        print("Connection established between nodes")
        return True

    def on_packet_connected(self, data):
        # TODO: Update timeout timer, that we have connectivity still
        if data == b'':
            return True  # this was a keepalive

        msg_type = chr(data[0])
        if msg_type == "p":
            pass
        else:
            return self.error("Unexpected dist message type: %s" % msg_type)

        return True

    def _send_packet2(self, content: bytes):
        """ Send a handshake-time status message with a 2 byte length prefix
        """
        print("Send handshake:", util.hex_bytes(content))
        msg = struct.pack(">H", len(content)) + content
        self.socket_.sendall(msg)

    def _send_challenge(self, my_challenge):
        print("Sending challenge (our number is %d)" % my_challenge,
              self.dist_.name_)
        msg = b'n' \
              + struct.pack(">HII",
                            epmd.DIST_VSN,
                            self.node_opts_.dflags_,
                            my_challenge) \
              + bytes(self.dist_.name_, "latin1")
        self._send_packet2(msg)

    @staticmethod
    def _check_digest(peer_digest: bytes,
                      peer_challenge: int,
                      cookie: str) -> bool:
        """ Hash cookie + the challenge together producing a verification hash
        """
        expected_digest = InConnection.make_digest(peer_challenge, cookie)
        # print("Check digest: expected digest", expected_digest,
        #      "peer digest", peer_digest)
        return peer_digest == expected_digest

    @staticmethod
    def make_digest(challenge: int, cookie: str) -> bytes:
        result = md5(bytes(cookie, "ascii")
                     + bytes(str(challenge), "ascii")).digest()
        print("make_digest cookie=", cookie, "challenge=", challenge,
              "result=", result)
        return result

    def _send_challenge_ack(self, peers_challenge: int, cookie: str):
        """ After cookie has been verified, send the confirmation by digesting
            our cookie with the remote challenge
        """
        digest = InConnection.make_digest(peers_challenge, cookie)
        self._send_packet2(b'a' + digest)


__all__ = ['InConnection']

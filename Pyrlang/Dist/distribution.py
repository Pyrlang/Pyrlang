from __future__ import print_function

import struct

from Pyrlang import ErlNode
from gevent import socket

NODE_HIDDEN = 77
NODE_NORMAL = 72
DIST_VSN = 5        # Distribution protocol version

REQ_ALIVE2 = 120
DIST_PROTOCOL = 0   # 0 = TCP/IP


class ErlDistribution:
    """ Implements network part of the EPMD registration and Erlang distribution
        protocol. Extends functionality of ErlNode, so all functions take 
        ErlNode as a parameter but don't store it to avoid creating a ref cycle    
    """

    def __init__(self, node: ErlNode, name: str, cookie: str) -> None:
        self.name_ = name
        self.cookie_ = cookie
        self.in_port_ = 0

        self.epmd_host_ = 'localhost'
        self.epmd_port_ = 4369
        self.epmd_sock_ = None

    def connect(self, node: ErlNode):
        """ Looks up EPMD daemon and connects to it trying to discover other 
            Erlang nodes.
        """
        self.epmd_sock_ = socket.create_connection((self.epmd_host_, self.epmd_port_))
        self.epmd_req_alive2(NODE_HIDDEN, (DIST_VSN, DIST_VSN), "")

    def disconnect(self):
        """
        :return:
        """
        self.epmd_sock_.close()

    def epmd_req_alive2(self, nodetype: int, dist_vsn: tuple, extra: str):
        extra = bytes(extra, "latin1")
        msg = struct.pack("B>HBB>H>H>Hs>Hs",
                          REQ_ALIVE2, self.in_port_, nodetype, DIST_PROTOCOL,
                          dist_vsn[0], dist_vsn[1],
                          len(self.name_), self.name_,
                          len(extra), extra)
        self.epmd_req(msg)

    def epmd_req(self, req: bytes):
        """ Generic helper function to send a preformatted request to EPMD
        """
        header = struct.pack(">H", len(req))
        self.epmd_sock_.send(header + req)

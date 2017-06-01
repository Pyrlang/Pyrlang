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

""" The module represents EPMD connection and implements the protocol.
    EPMD is a daemon application, part of Erlang/OTP which registers Erlang
    nodes on the local machine and helps nodes finding each other.
"""
import struct
from typing import Union

import gevent
import sys
from gevent import socket

from Pyrlang import logger
from Pyrlang.Dist import util, dist_protocol

NODE_HIDDEN = 77
NODE_NORMAL = 72

RESP_PORT2 = 119
REQ_ALIVE2 = 120
REQ_PORT_PLEASE2 = 122

DIST_PROTOCOL = 0  # 0 = TCP/IP

PY3 = sys.version_info[0] >= 3

EPMD_DEFAULT_PORT = 4369
EPMD_REMOTE_DEFAULT_TIMEOUT = 5.0

LOG = logger.nothing
WARN = logger.nothing
ERROR = logger.tty


class EPMDClientError(Exception):
    pass


class EPMDConnectionError(Exception):
    pass


class EPMDClient:
    """ An EPMD client connection which registers ourselves in EPMD and can
        do other queries.
    """

    def __init__(self) -> None:
        self.host_ = '127.0.0.1'
        """ The local EPMD is always located on the local host. """
        self.port_ = EPMD_DEFAULT_PORT

        self.sock_ = None  # network socket

    def close(self):
        """ Closing EPMD connection removes the node from available EPMD nodes
            list.
        """
        print("EPMD: Close")
        self.sock_.close()
        self.sock_ = None

    def connect(self) -> bool:
        """ Establish a long running connection to EPMD, will not return until
            the connection has been established.

            :return: True
        """
        while True:
            try:
                print("EPMD: Connecting %s:%d" % (self.host_, self.port_))
                host_port = (self.host_, self.port_)
                self.sock_ = socket.create_connection(address=host_port,
                                                      timeout=5.0)
                break  # the connect loop

            except socket.error as err:
                print("EPMD: connection error:", err)
                gevent.sleep(5)

        print("EPMD: Socket connected")
        return True

    def alive2(self, dist) -> bool:
        """ Send initial hello (ALIVE2) to EPMD

            :param dist: The distribution object from the node
            :return: Success True or False
        """
        self._req_alive2(nodetype=NODE_HIDDEN,
                         node_name=dist.name_,
                         in_port=dist.in_port_,
                         dist_vsn=dist_protocol.DIST_VSN_PAIR,
                         extra="")

        creation = self._read_alive2_reply()
        if creation >= 0:
            print("EPMD: Connected successfully (creation %d)"
                  % creation)
            dist.creation_ = creation
            return True

        print("EPMD: ALIVE2 failed with creation %d" % creation)
        return False

    def _read_alive2_reply(self) -> int:
        """ Read reply from ALIVE2 request, check the result code, read creation

            :return: Creation value if all is well, connection remains on.
                On error returns -1
        """
        # Reply will be [121,0,Creation:16] for OK, otherwise [121,Error]
        reply = self.sock_.recv(2)
        if not reply:
            print("EPMD: ALIVE2 Read error. Closed?", reply)
            return -1

        if reply[1] == 0:
            cr = self.sock_.recv(2)
            (creation,) = struct.unpack(">H", cr)
            return creation

        print("EPMD: ALIVE2 returned error", reply[1])
        return -1

    @staticmethod
    def _make_req_alive2(nodetype: int, name: str, in_port: int,
                         dist_vsn: tuple, extra: str):
        extra = bytes(extra, "latin1")
        name = bytes(name.split("@")[0], "utf8")

        # Here let's tell EPMD that we've arrived, our protocols and our name
        msg1 = struct.pack(">BH BB HH",
                           REQ_ALIVE2, in_port,
                           nodetype, DIST_PROTOCOL,
                           dist_vsn[0], dist_vsn[1])
        msg2 = struct.pack(">H", len(name)) + name
        msg3 = struct.pack(">H", len(extra)) + extra

        return msg1 + msg2 + msg3

    def _req_alive2(self, nodetype: int, node_name: str, in_port: int,
                    dist_vsn: tuple, extra: str):
        msg = self._make_req_alive2(nodetype, node_name, in_port,
                                    dist_vsn, extra)
        print("EPMD: sending ALIVE2 req", (node_name, nodetype, dist_vsn))
        self._req(msg)

    def _req(self, req: bytes):
        """ Generic helper function to send a preformatted request to EPMD
        """
        header = struct.pack(">H", len(req))
        self.sock_.sendall(header + req)

    @staticmethod
    def query_node(node: str) -> tuple:
        """ Query EPMD about the port to the given node.

            :param node: String with node "name@ip" or "name@hostname"
            :return: Host and port where the node is, or None
            :rtype: tuple(str, int)
            :throws EPMDClientError: if something is wrong with input
            :throws EPMDConnectionError: if connection went wrong
        """
        # Trim the host name/IP after the @ and resolve the DNS name
        if "@" not in node:
            raise EPMDClientError("Node must have @ in it")

        (r_name, r_ip_or_hostname) = node.split("@")
        r_ip = socket.gethostbyname(r_ip_or_hostname)

        port_please2 = bytes([REQ_PORT_PLEASE2]) \
            + bytes(r_name, "utf8")  # not sure if latin-1 here

        resp = EPMDClient._fire_forget_query(r_ip, port_please2)

        # RESP_PORT2
        # Response Error structure
        # 1     1
        # 119   Result > 0
        if len(resp) < 2 or resp[0] != RESP_PORT2:
            ERROR("EPMD: PORT_PLEASE2 to %s sent wrong response %s"
                  % (r_ip, resp))
            raise EPMDConnectionError("PORT_PLEASE2 wrong response")

        if resp[1] != 0:
            ERROR("EPMD: PORT_PLEASE2 to %s: error %d" % (r_ip, resp[1]))
            raise EPMDConnectionError("PORT_PLEASE2 error %d" % resp[1])

        # Response structure
        # 1     1       2       1           1           2               ...
        # 119   Result  PortNo  NodeType    Protocol    HighestVersion  ...
        # 2             2       Nlen        2       Elen
        # LowestVersion Nlen    NodeName    Elen    >Extra

        r_port = util.u16(resp, 2)
        # node_type = resp[4]
        # protocol = resp[5]
        versions = (util.u16(resp, 6), util.u16(resp, 8))
        if not dist_protocol.dist_version_check(versions):
            raise EPMDConnectionError(
                "Remote node %s supports protocol version %s and we "
                "support %d" % (node, versions, dist_protocol.DIST_VSN))

        # ignore node name and extra

        return r_ip, r_port

    @staticmethod
    def _fire_forget_query(ip: str, query: bytes) -> bytes:
        """ Connect to node, fire the query, read and disconnect. """
        s = socket.create_connection(address=(ip, EPMD_DEFAULT_PORT),
                                     timeout=EPMD_REMOTE_DEFAULT_TIMEOUT)
        query1 = util.to_u16(len(query)) + query
        s.send(query1)

        # Expect that after everything is received, the peer will close
        # the socket automatically, so we will too
        result = b''
        while True:
            incoming = s.recv(4096)
            if incoming == b'':
                break

            result += incoming

        s.close()
        return result


__all__ = ['EPMDClient']

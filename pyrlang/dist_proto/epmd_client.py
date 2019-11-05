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

""" The module represents EPMD connection and implements the protocol.
    EPMD is a daemon application, part of Erlang/OTP which registers Erlang
    nodes on the local machine and helps nodes finding each other.
"""
import asyncio
import logging
import socket
import struct
import sys
from typing import Tuple

from pyrlang.dist_proto import version
from pyrlang.errors import EPMDClientError, EPMDConnectionError
from term import util

LOG = logging.getLogger(__name__)

NODE_HIDDEN = 77
NODE_NORMAL = 72

RESP_PORT2 = 119
REQ_ALIVE2 = 120
REQ_PORT_PLEASE2 = 122

DIST_PROTOCOL = 0  # 0 = TCP/IP

PY3 = sys.version_info[0] >= 3

EPMD_DEFAULT_PORT = 4369
EPMD_REMOTE_DEFAULT_TIMEOUT = 5.0


class EPMDClient(asyncio.Protocol):
    """ An EPMD client connection which registers ourselves in EPMD and can
        do other queries.
    """

    def __init__(self) -> None:
        self.host_ = '127.0.0.1'
        """ The local EPMD is always located on the local host. """

        self.port_ = EPMD_DEFAULT_PORT
        """ We expect local EPMD to be available on the default port. """

        self.reader_ = None  # type: [asyncio.StreamReader, None]
        self.writer_ = None  # type: [asyncio.StreamWriter, None]
        self.n_connection_attempts_ = 5

    def close(self):
        """ Closing EPMD connection removes the node from available EPMD nodes
            list.
        """
        LOG.info("Closing EPMD socket")
        self.writer_.close()

    async def connect(self) -> bool:
        """ Establish a long running connection to EPMD, will not return until
            the connection has been established.

            :rtype: True if connection succeeded within 5 tries, else False
        """
        # Try to connect for N tries then fail
        for n_try in range(self.n_connection_attempts_):
            try:
                LOG.info("Connecting to EPMD %s:%d", self.host_, self.port_)

                self.reader_, self.writer_ = await asyncio.open_connection(
                    host=self.host_,
                    port=self.port_
                )
                LOG.info("EPMD socket connected")
                return True

            except Exception as err:
                LOG.error("EPMD connection error %s. Is local EPMD running? "
                          "Try `epmd -daemon`", err)
                await asyncio.sleep(5.0)

        LOG.error("Could not connect to EPMD in %d tries" %
                  self.n_connection_attempts_)
        return False

    async def alive2(self, dist) -> bool:
        """ Send initial hello (ALIVE2) to EPMD

            :type dist: pyrlang.dist.distribution.ErlangDistribution
            :param dist: The distribution object from the node
            :rtype: bool
        """
        assert isinstance(dist.in_port_, int)
        await self._req_alive2(nodetype=NODE_HIDDEN,
                               node_name=dist.node_name_,
                               in_port=dist.in_port_,
                               dist_vsn=version.DIST_VSN_PAIR,
                               extra="")

        creation = await self._read_alive2_reply()
        if creation >= 0:
            LOG.info("Connected successfully (creation %d)", creation)
            dist.creation_ = creation
            return True

        LOG.error("ALIVE2 failed with creation %d", creation)
        return False

    async def _read_alive2_reply(self) -> int:
        """ Read reply from ALIVE2 request, check the result code, read creation

            :return: Creation value if all is well, connection remains on.
                On error returns -1
        """
        # Reply will be [121,0,Creation:16] for OK, otherwise [121,Error]
        reply = await self.reader_.read(2)
        if not reply:
            LOG.error("ALIVE2 Read error. Closed? %s", reply)
            return -1

        if reply[1] == 0:
            cr = await self.reader_.read(2)
            (creation,) = struct.unpack(">H", cr)
            return creation

        LOG.error("ALIVE2 returned error %s", reply[1])
        return -1

    @staticmethod
    def _make_req_alive2(nodetype: int, name0: str, in_port: int,
                         dist_vsn: tuple, extra0: str):
        extra = bytes(extra0, "latin1")
        name = bytes(name0.split("@")[0], "utf8")

        # Here let's tell EPMD that we've arrived, our protocols and our name
        msg1 = struct.pack(">BH BB HH",
                           REQ_ALIVE2, in_port,
                           nodetype, DIST_PROTOCOL,
                           dist_vsn[0], dist_vsn[1])
        msg2 = struct.pack(">H", len(name)) + name
        msg3 = struct.pack(">H", len(extra)) + extra

        return msg1 + msg2 + msg3

    async def _req_alive2(self, nodetype: int, node_name: str, in_port: int,
                          dist_vsn: tuple, extra: str):
        msg = self._make_req_alive2(nodetype, node_name, in_port,
                                    dist_vsn, extra)
        LOG.debug("sending ALIVE2 req n=%s (%s) vsn=%s",
                  node_name, nodetype, dist_vsn)
        await self._req(msg)

    async def _req(self, req: bytes):
        """ Generic helper function to send a preformatted request to EPMD
        """
        header = struct.pack(">H", len(req))
        self.writer_.write(header + req)

    @staticmethod
    async def query_node(node_name: str) -> [Tuple[str, int], None]:
        """ Query EPMD about the port to the given node.

            :param node_name: String with node "name@ip" or "name@hostname"
            :return: Host and port where the node is, or None if not found
            :rtype: [tuple(str, int), None]
            :throws EPMDClientError: if something is wrong with input
            :throws EPMDConnectionError: if connection went wrong
        """
        # Trim the host name/IP after the @ and resolve the DNS name
        if "@" not in node_name:
            raise EPMDClientError("Node must have @ in it")

        (r_name, r_ip_or_hostname) = node_name.split("@")
        r_ip = socket.gethostbyname(r_ip_or_hostname)

        # not sure if latin-1 here
        port_please2 = bytes([REQ_PORT_PLEASE2]) + bytes(r_name, "utf8")

        resp = await EPMDClient._fire_forget_query(r_ip, port_please2)

        # RESP_PORT2
        # Response Error structure
        # 1     1
        # 119   Result > 0
        if len(resp) < 2 or resp[0] != RESP_PORT2:
            raise EPMDConnectionError(
                "PORT_PLEASE2 to %s sent wrong response %s", r_ip, resp)

        if resp[1] != 0:
            # raise EPMDConnectionError(
            #     "PORT_PLEASE2 to %s: error %d" % (r_ip, resp[1]))
            LOG.info("EPMD: Node '%s' is not known to EPMD on %s",
                     node_name, r_ip)
            return None

        # Response structure
        # 1     1       2       1           1           2               ...
        # 119   Result  PortNo  NodeType    Protocol    HighestVersion  ...
        # 2             2       Nlen        2       Elen
        # LowestVersion Nlen    NodeName    Elen    >Extra

        r_port = util.u16(resp, 2)
        node_type = resp[4]
        protocol = resp[5]
        versions = (util.u16(resp, 6), util.u16(resp, 8))
        LOG.info("EPMD: Node %s is at %s:%d (type=%d proto=%d)",
                 node_name, r_ip, r_port, node_type, protocol)
        if not version.dist_version_check(versions):
            raise EPMDConnectionError(
                "Remote node %s supports protocol version %s and we "
                "support %d", node_name, versions, version.DIST_VSN)

        # ignore node name and extra

        return r_ip, r_port

    @staticmethod
    async def _fire_forget_query(ip: str, query: bytes) -> bytes:
        """ Connect to node, fire the query, read and disconnect. """
        try:
            reader, writer = await asyncio.open_connection(
                host=ip,
                port=EPMD_DEFAULT_PORT,
                # timeout=EPMD_REMOTE_DEFAULT_TIMEOUT
            )
        except Exception as err:
            LOG.error(str(err))
            raise err

        query1 = util.to_u16(len(query)) + query
        writer.write(query1)

        # Expect that after everything is received, the peer will close
        # the socket automatically, so we will too
        result = b''
        while True:
            incoming = await reader.read(4096)
            if incoming == b'':
                break

            result += incoming

        writer.close()
        return result


__all__ = ['EPMDClient']

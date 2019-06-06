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

""" Distribution class is a separate running Task which owns its TCP connection.
"""
import asyncio
import logging

import pyrlang2
from pyrlang2.dist_proto import DistClientProtocol
from pyrlang2.dist_proto.epmd_client import EPMDClient

LOG = logging.getLogger("pyrlang.dist")


class ErlangDistribution:
    """ Implements network part of the EPMD registration and Erlang dist_proto
        protocol.
    """

    def __init__(self, node_name: str) -> None:
        self.node_name_ = node_name
        """ Node name, a string. """

        self.creation_ = 0
        """ Creation id used in pid generation. EPMD gives creation id to 
            newly connected nodes. 
        """

        self.in_port_ = None  # type: [None, int]
        self.in_srv_ = None  # type: [None, asyncio.AbstractServer]
        self.epmd_ = EPMDClient()
        self.n_connect_tries_ = 5

    async def start_server(self):
        """ Start listening for incoming connections.
        """
        # Create handler using make_handler_in helper
        # proto_kwargs = {"node_name": node_name}

        from pyrlang2.dist_proto.server import DistServerProtocol
        self.in_srv_ = await asyncio.get_event_loop().create_server(
            host='0.0.0.0',
            port=0,
            protocol_factory=DistServerProtocol
        )
        _, self.in_port_ = self.in_srv_.sockets[0].getsockname()
        LOG.info("Listening for dist connections on port %s", self.in_port_)

    async def start_distribution(self) -> bool:
        """ 1. Starts local Erlang distribution server on a random port
            2. Looks up EPMD daemon and connects to it trying to discover other
                Erlang nodes.
        """
        await self.start_server()
        await self.epmd_.connect()
        return await self.epmd_.alive2(self)

    def disconnect_epmd(self) -> None:
        """ Finish EPMD connection, this will remove the node from the list of
            available nodes on EPMD
        """
        self.in_srv_.close()
        self.epmd_.close()

    def destroy(self):
        LOG.info("Stopping dist service")
        self.disconnect_epmd()
        del self.epmd_

    async def connect_to_node(self,
                              local_node: str,
                              remote_node: str) -> bool:
        """ Query EPMD where is the node, and initiate dist connection.

            :param local_node: name of the local Erlang Node object (will be
                reached via `Node.all_nodes[local_node]`)
            :param remote_node: String with node 'name@ip'
            :return: boolean whether the connection succeeded
        """
        host_port = await EPMDClient.query_node(remote_node)
        if host_port is None:
            # Connection to node failed, node is not known
            return False
        else:
            _future = await asyncio.get_event_loop().create_connection(
                DistClientProtocol,
                host=host_port[0],
                port=host_port[1]
            )
            return True


__all__ = ['ErlangDistribution']

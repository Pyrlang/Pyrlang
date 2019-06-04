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

import logging

from pyrlang2.dist_proto import EPMDClient
from pyrlang2.dist_proto import OutDistProtocol

LOG = logging.getLogger("pyrlang.dist")


class ErlangDistribution:
    """ Implements network part of the EPMD registration and Erlang dist_proto
        protocol.
    """

    def __init__(self, node_name: str, engine: BaseEngine) -> None:
        self.engine_ = engine
        """ Async adapter engine for network and timer operations implemented 
            either as Gevent or asyncio """

        self.node_name_ = node_name
        """ Node name, a string. """

        self.creation_ = 0
        """ Creation id used in pid generation. EPMD gives creation id to 
            newly connected nodes. 
        """

        # Listener for Incoming connections from other nodes
        # Create handler using make_handler_in helper
        proto_kwargs = {"node_name": node_name,
                        "engine": engine}

        from pyrlang.dist.in_dist_protocol import InDistProtocol
        (self.in_srv_, self.in_port_) = self.engine_.listen_with(
            protocol_class=InDistProtocol,
            protocol_args=[],
            protocol_kwargs=proto_kwargs
        )
        LOG.info("Listening for dist connections on port %s", self.in_port_)

        self.epmd_ = EPMDClient(engine)

    async def connect(self) -> bool:
        """ Looks up EPMD daemon and connects to it trying to discover other 
            Erlang nodes.
        """
        while True:
            if await self.epmd_.connect():
                return self.epmd_.alive2(self)

            self.engine_.sleep(5.0)

    def disconnect(self) -> None:
        """ Finish EPMD connection, this will remove the node from the list of
            available nodes on EPMD
        """
        self.in_srv_.close()
        self.epmd_.close()

    def destroy(self):
        LOG.info("Stopping dist service")
        self.disconnect()
        del self.epmd_

    def connect_to_node(self, local_node: str, remote_node: str,
                        engine: BaseEngine):
        """ Query EPMD where is the node, and initiate dist connection. Blocks
            the Greenlet until the connection is made or have failed.

            :param engine: Async engine adapter (GeventEngine or AsyncioEngine)
            :param local_node: Reference to Erlang Node object
            :param remote_node: String with node 'name@ip'
            :return: Handler or None
        """
        try:
            host_port = self.epmd_.query_node(remote_node)
            (handler, _sock) = self.engine_.connect_with(
                protocol_class=OutDistProtocol,
                protocol_args=[],
                protocol_kwargs={"node_name": local_node, "engine": engine},
                host_port=host_port
            )
            return handler

        except Exception as e:
            LOG.error("Dist: " + str(e))
            return None


__all__ = ['ErlangDistribution']

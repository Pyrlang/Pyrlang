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

""" Distribution class is not a separate running Greenlet, but rather a helper,
    which is called upon.
"""

from __future__ import print_function

import gevent
from gevent.server import StreamServer

from Pyrlang import logger
from Pyrlang.Dist import helpers
from Pyrlang.Dist.epmd import EPMDClient, EPMDConnectionError
from Pyrlang.Dist.out_connection import OutConnection

LOG = logger.nothing
WARN = logger.nothing
ERROR = logger.tty


class ErlangDistribution:
    """ Implements network part of the EPMD registration and Erlang distribution
        protocol. Extends functionality of Node, so all functions take
        Node as a parameter but don't store it to avoid creating a ref cycle.
    """

    def __init__(self, node, name: str) -> None:
        self.name_ = name
        """ Node name, a string. """

        self.creation_ = 0
        """ Creation id used in pid generation. EPMD gives creation id to 
            newly connected nodes. 
        """

        # Listener for Incoming connections from other nodes
        # Create handler using make_handler_in helper
        proto_kwargs = {"node": node}

        from Pyrlang.Dist.in_connection import InConnection
        handler = helpers.make_handler_in(receiver_class=InConnection,
                                          args=[],
                                          kwargs=proto_kwargs)

        self.in_srv_ = StreamServer(listener=('127.0.0.1', 0),
                                    handle=handler)
        self.in_srv_.start()
        self.in_port_ = self.in_srv_.server_port
        print("Dist: Listening for dist connections on port", self.in_port_)

        self.epmd_ = EPMDClient()

    def connect(self, node) -> bool:
        """ Looks up EPMD daemon and connects to it trying to discover other 
            Erlang nodes.
        """
        while True:
            if self.epmd_.connect():
                return self.epmd_.alive2(self)

            gevent.sleep(5)

    def disconnect(self) -> None:
        """ Finish EPMD connection, this will remove the node from the list of
            available nodes on EPMD
        """
        self.epmd_.close()

    def connect_to_node(self, this_node, remote_node: str):
        """ Query EPMD where is the node, and initiate dist connection. Blocks
            the Greenlet until the connection is made or have failed.

            :type this_node: Pyrlang.Node
            :param this_node: Reference to Erlang Node object
            :param remote_node: String with node 'name@ip'
            :return: Handler or None
        """
        try:
            host_port = EPMDClient.query_node(remote_node)
            (handler, _sock) = helpers.connect_with(
                protocol_class=OutConnection,
                host_port=host_port,
                args=[],
                kwargs={"node": this_node}
            )
            return handler

        except Exception as e:
            ERROR("Dist:", e)
            return None


__all__ = ['ErlangDistribution']

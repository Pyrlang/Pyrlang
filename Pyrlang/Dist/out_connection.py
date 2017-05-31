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

""" The module implements outgoing TCP distribution connection (i.e. initiated
    by our node to another node with the help of EPMD).
"""

from __future__ import print_function

from Pyrlang.Dist.base_connection import BaseConnection
from Pyrlang.Dist.node_opts import NodeOpts


class OutConnection(BaseConnection):
    """ Handles outgoing connections from our to other nodes.

        Behaves like a ``Greenlet`` but the actual recv loop around this
        protocol is located in the ``util.connect_with`` helper function.
    """
    DISCONNECTED = 'disconn'
    CONNECTED = 'conn'
    RECV_STATUS = 'recv_status'

    def __init__(self, dist, node_opts: NodeOpts):
        BaseConnection.__init__(self, dist, node_opts)
        self.state_ = self.DISCONNECTED

    def on_packet(self, data) -> bool:
        pass

    def on_connected(self, sockt, address):
        BaseConnection.on_connected(self, sockt=sockt, address=address)
        self.state_ = self.RECV_STATUS

    def on_connection_lost(self):
        BaseConnection.on_connection_lost(self)
        self.state_ = self.DISCONNECTED

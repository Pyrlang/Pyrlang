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
import asyncio
import logging
from typing import Dict, Set

from pyrlang2.dist_proto import DistributionFlags, ErlangDistribution
from term import Pid, Atom

LOG = logging.getLogger("pyrlang")


class Node:
    """ Implements an Erlang node which has a network name, a dictionary of
        processes and registers itself via EPMD.
        Node handles the networking asynchronously.

        This is the root object of an Erlang node, it must be created first and
        must outlive all other objects it manages, for them to be accessible
        over the network.

        Example:

        1. Create an async engine adapter:
            e = GeventEngine()  # located in `import Pyrlang`

        2. Create a node class with a name and a cookie
            ``node = Pyrlang.Node("py@127.0.0.1", "COOKIE", engine=e)``

        3. Start it via the engine adapter ``e.start(node)``

        4. Now anything that you do (for example an infinite loop with
            ``e.sleep(1)`` in it, will give CPU time to the node.
    """

    all_nodes = {}  # type: Dict[str, Node]
    """ All existing local Node objects indexed by node_name: str """

    def __init__(self, node_name: str, cookie: str) -> None:
        """ Sets up the new node, initiating EPMD connection as necessary
            :param node_name: str
            :param cookie: str
        """
        self.node_name_ = node_name  # type: str
        """ Node name as seen on the network. Use full node names here:
            ``name@hostname`` """

        Node.all_nodes[node_name] = self

        self.inbox_ = asyncio.Queue()
        """ Message queue is awaited, and the receive handler is called
            if anything arrives. """

        self.pid_counter_ = 0
        """ An internal counter used to generate unique process ids """

        self.processes_ = {}  # type: Dict[Pid, Process]
        """ Process dictionary which stores all the existing ``Process`` objects
            adressable by a pid.
    
            .. note:: This creates a python reference to an
                object preventing its automatic garbage collection.
                In the end of its lifetime an object must be explicitly removed
                from this dictionary using ``Process.exit`` method on the
                process.
        """

        self.reg_names_ = {}  # type: Dict[Atom, Pid]
        """ Registered objects dictionary, which maps atoms to pids
        """

        self.is_exiting_ = False

        self.node_opts_ = DistributionFlags(cookie=cookie)
        """ Distribution options object with feature support flags packed into 
            an integer. The remote node will receive these flags to know what
            features we can support.
        """

        self._signal_wakeups = set()  # type: Set[Pid]

        self.dist_nodes_ = {}  # type: Dict[str, BaseDistProtocol]
        self.dist_ = ErlangDistribution(node_name=node_name)

        # This is important before we can begin spawning processes
        # to get the correct node creation
        self.dist_.connect()

        # Spawn and register (automatically) the process 'rex' for remote
        # execution, which takes 'rpc:call's from Erlang
        from pyrlang.rex import Rex
        self.rex_ = Rex(node=self)

        # Spawn and register (automatically) the 'net_kernel' process which
        # handles special ping messages
        from pyrlang.net_kernel import NetKernel
        self.net_kernel_ = NetKernel(node=self)

        asyncio.create_task(self._async_loop())

    async def _async_loop(self):
        pass

# Copyright 2018, Erlang Solutions Ltd.
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

from Pyrlang.Engine.base_engine import BaseEngine


class BasePid:
    pass


class BaseNode:
    """ A code-less base for Erlang Node used to break circular imports and
        simplify type speccing of Node arguments and variables. """

    def __init__(self, node_name: str, engine: BaseEngine):
        self.engine_ = engine  # type: BaseEngine
        """ Async adapter engine for network and timer operations implemented 
            either as Gevent or asyncio """

        self.node_name_ = node_name  # type: str
        """ Node name as seen on the network. Use full node names here:
            ``name@hostname`` """

    def register_new_process(self, proc) -> BasePid:
        raise NotImplementedError()

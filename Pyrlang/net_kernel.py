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

import logging

from Pyrlang.Term.atom import Atom
from Pyrlang.gen_server import GenServer
from Pyrlang.node import Node

LOG = logging.getLogger("Pyrlang")


class NetKernel(GenServer):
    """ A special process which registers itself as ``net_kernel`` and handles
        one specific ``is_auth`` message, which is used by ``net_adm:ping``.
    """

    def __init__(self, node: Node) -> None:
        GenServer.__init__(self, node, accepted_calls=['is_auth'])
        node.register_name(self, Atom('net_kernel'))

    @staticmethod
    def is_auth():
        return Atom('yes')


__all__ = ['NetKernel']

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

import logging

from pyrlang2.gen_server import GenServer
from term.atom import Atom

LOG = logging.getLogger("pyrlang")


class NetKernel(GenServer):
    """ A special process which registers itself as ``net_kernel`` and handles
        one specific ``is_auth`` message, which is used by ``net_adm:ping``.
    """

    def __init__(self, node) -> None:
        """ :param node: pyrlang2.node.Node
        """
        GenServer.__init__(self,
                           node_name=node.node_name_,
                           accepted_calls=['is_auth'])
        node.register_name(self, Atom('net_kernel'))

    @staticmethod
    def is_auth():
        return Atom('yes')


__all__ = ['NetKernel']

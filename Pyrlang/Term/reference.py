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

import struct

from Pyrlang.Dist import util
from Pyrlang.Term.atom import Atom


class Reference:
    """ Represents a reference value from Erlang, typically it has 12 bytes of
        unique data, but it might change.
    """

    def __init__(self, node_name: Atom, creation: int, refid: bytes) -> None:
        # Node the ref comes from
        self.node_name_ = node_name
        # Identification bytes, guaranteed to be unique on the creating node
        self.id_ = refid
        self.creation_ = creation

    def __repr__(self) -> str:
        # Assume that ref has only 3 32-bit words (actually id size is not
        # specified in docs and can be a different multiple of 4)
        if len(self.id_) == 12:
            v = struct.unpack(">III", self.id_)
            return "Ref<%d,%d,%d,%d>@%s" % \
                   (self.creation_, v[0], v[1], v[2], self.node_name_.text_)
        else:
            return "Ref<%d,%s>" % (self.creation_,
                                   util.hex_bytes(self.id_, ","))

    def __str__(self) -> str:
        return self.__repr__()

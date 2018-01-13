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

from Pyrlang.Dist import util


class Binary:
    """ Represents a bytes object, with last byte optionally incomplete.
        Refer to the ``bytes_`` field but remember that for bit objects
        the ``last_byte_bits_`` field can be less than 8.
    """

    def __init__(self, data: bytes, last_byte_bits: int = 8) -> None:
        self.bytes_ = data
        self.last_byte_bits_ = last_byte_bits

    def __repr__(self) -> str:
        lbb = self.last_byte_bits_
        if lbb == 8:
            return "<<%s>>" % util.dec_bytes(self.bytes_, ",")
        else:
            return "<<%s:%d>>" % (util.dec_bytes(self.bytes_, ","), lbb)

    def __str__(self) -> str:
        return self.__repr__()

    def equals(self, other) -> bool:
        return isinstance(other, Binary) \
               and self.bytes_ == other.bytes_ \
               and self.last_byte_bits_ == other.last_byte_bits_

    __eq__ = equals

    def __ne__(self, other):
        return not self.equals(other)


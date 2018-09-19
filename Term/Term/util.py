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


def u16(data: bytes, pos: int = 0):
    return struct.unpack(">H", data[pos:pos + 2])[0]


def u32(data: bytes, pos: int = 0):
    return struct.unpack(">I", data[pos:pos + 4])[0]


def i32(data: bytes, pos: int = 0):
    return struct.unpack(">i", data[pos:pos + 4])[0]


def to_u32(val):
    return struct.pack(">I", val)


def to_i32(val):
    return struct.pack(">i", val)


def to_u16(val):
    return struct.pack(">H", val)


def hex_bytes(data: bytes, sep: str= " "):
    """ Format a bytes() object as a hex dump """
    return sep.join("{:02x}".format(bval) for bval in data)


def dec_bytes(data: bytes, sep: str= " "):
    """ Format a bytes() object as a decimal dump """
    return sep.join(str(bval) for bval in data)


__all__ = ['hex_bytes', 'dec_bytes',
           'u16', 'u32', 'i32',
           'to_u16', 'to_u32', 'to_i32']

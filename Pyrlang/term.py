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

""" Module Term implements Erlang term wrappers which expose Erlang-like
    behaviour and hide details.
"""

from __future__ import print_function

import struct

#from future.utils import python_2_unicode_compatible
from builtins import chr

from Pyrlang.Dist import util

ATOM_MARKER = "pyrlang.Atom"
PID_MARKER = "pyrlang.Pid"


#@python_2_unicode_compatible
class Atom:
    """ Stores a string decoded from Erlang atom. Encodes back to atom.
        Can serve as a Python dictionary key.
    """
    def __repr__(self) -> str:
        return "atom'%s'" % self.text_

    def __str__(self):
        return self.text_

    def __init__(self, text: str) -> None:
        self.text_ = text

    def equals(self, other) -> bool:
        return isinstance(other, Atom) and self.text_ == other.text_

    __eq__ = equals

    def __ne__(self, other):
        return not self.equals(other)

    def __hash__(self):
        return hash((ATOM_MARKER, self.text_))


class List:
    """ Erlang list wrapper which stores elements in a Python list.
        The list can have a ``tail_`` for improper list representation,
        and can be interpreted as a Python string optionally.
        Get ``elements_`` field directly if you want the list value.
    """

    def __repr__(self) -> str:
        if self.tail_ == []:
            return str(self.elements_)

        elements = ", ".join(str(e) for e in self.elements_)
        return "[%s | %s]" % (elements, self.tail_)

    def __init__(self) -> None:
        self.elements_ = []
        """ List value containing other Erlang and Python values. """
        self.tail_ = []
        """ Optional tail value, if you are sure that the list is properly
            formed, you can ignore this field. In properly formed lists it 
            is always ``nil`` (``[]``).
        """

    def __str__(self) -> str:
        return self.__repr__()

    def append(self, x):
        self.elements_.append(x)

    def set_tail(self, t):
        """ Sets the tail element. For proper lists this must always be [].
            This field is used when encoding/decoding network representation
            for the list. Python code can optionally check or set it.
        """
        self.tail_ = t

    def as_unicode(self) -> str:
        """ Builds an unicode string from integers in the list, assuming that
            not all of them are bytes and some of them can be large Unicode
            codepoints.
        """
        return "".join([chr(x) for x in self.elements_])


class Pid:
    """ Represents Erlang-style process identifier with 3 components. Node
        component is always 0 for local node, otherwise it can take arbitrary
        integer values.

        A pid is created by Erlang automatically. Pyrlang Process also generates
        one when it is born to register itself in the process registry. Pid
        uniquely identifies a running process in the cluster.
    """
    def __init__(self, node: Atom, id: int, serial: int, creation: int) -> None:
        self.node_ = node
        self.id_ = id
        self.serial_ = serial
        self.creation_ = creation

    def __repr__(self) -> str:
        return "Pid<%d.%d.%d>@%s" % (self.creation_, self.id_, self.serial_,
                                     self.node_.text_)

    def __str__(self) -> str:
        return self.__repr__()

    def equals(self, other) -> bool:
        return isinstance(other, Pid) \
               and self.node_ == other.node_ \
               and self.id_ == other.id_ \
               and self.serial_ == other.serial_ \
               and self.creation_ == other.creation_

    __eq__ = equals

    def __ne__(self, other):
        return not self.equals(other)

    def __hash__(self):
        return hash((PID_MARKER, self.node_,
                     self.id_, self.serial_, self.creation_))


class Reference:
    """ Represents a reference value from Erlang, typically it has 12 bytes of
        unique data, but it might change.
    """
    def __init__(self, node: Atom, creation: int, id: bytes) -> None:
        self.node_ = node
        """ Node the ref comes from. """
        self.id_ = id
        """ Identification bytes, guaranteed to be unique on the creating node.
        """
        self.creation_ = creation

    def __repr__(self) -> str:
        # Assume that ref has only 3 32-bit words (actually id size is not
        # specified in docs and can be a different multiple of 4)
        if len(self.id_) == 12:
            v = struct.unpack(">III", self.id_)
            return "Ref<%d,%d,%d,%d>@%s" % \
                   (self.creation_, v[0], v[1], v[2], self.node_.text_)
        else:
            return "Ref<%d,%s>" % (self.creation_,
                                   util.hex_bytes(self.id_, ","))

    def __str__(self) -> str:
        return self.__repr__()


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


__all__ = ['Atom', 'Pid', 'Binary', 'Reference', 'List']

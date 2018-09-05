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

ATOM_MARKER = "pyrlang.Atom"


class Atom:
    """ Stores a string decoded from Erlang atom. Encodes back to atom.
        Can serve as a Python dictionary key.
    """

    def __init__(self, text: str) -> None:
        self.text_ = text
        """ Atom's text representation, supports unicode """

    def __repr__(self) -> str:
        return "atom'%s'" % self.text_

    def __str__(self):
        return self.text_

    def equals(self, other) -> bool:
        return isinstance(other, Atom) and self.text_ == other.text_

    __eq__ = equals

    def __ne__(self, other):
        return not self.equals(other)

    def __hash__(self):
        return hash((ATOM_MARKER, self.text_))

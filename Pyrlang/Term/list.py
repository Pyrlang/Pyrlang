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

from typing import List

import array

NIL = []  # type: List


class ImproperList:
    """ A simple data holder used to pass improper lists back to Erlang.
        An Erlang improper list looks like `[1, 2, 3 | 4]` where 4 takes
        tail slot of last list cell instead of `NIL`. This is a rare data type
        and very likely you will not ever need it.
    """
    def __init__(self, elements, tail):
        self.elements_ = elements
        self.tail_ = tail


def list_to_unicode_str(lst):
    """ A helper function to convert a list of large integers incoming from
        Erlang into a unicode string. """
    return "".join(map(chr, lst))


def list_to_str(lst):
    """ A helper function to convert a list of bytes (0..255) into an
        ASCII string. """
    return array.array('B', lst).tostring()

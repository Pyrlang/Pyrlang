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


class Fun:
    """ Represents a pointer to a function in Erlang, with some variable
        values captured. Not callable from Python. """

    def __init__(self, mod, arity, pid, index, uniq, old_index, old_uniq, free):
        # Pid of the Erlang process which created the function
        self.pid_ = pid

        self.arity_ = arity

        # Atom which contains the code for this function
        self.module_ = mod

        self.old_index_ = old_index

        # Internal index, an integer, in the module's internal fun table.
        # Has no meaning in Python
        self.index_ = index

        # An integer with the hash value of the function parse
        self.old_uniq_ = old_uniq

        # An md5 hash of the significant parts of the BEAM file
        self.uniq_ = uniq

        # A list of values: frozen captured variables
        self.free_ = free

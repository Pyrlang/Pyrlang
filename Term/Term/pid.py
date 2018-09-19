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

from Term.bases import BasePid

PID_MARKER = "pyrlang.Pid"


class Pid(BasePid):
    """ Represents Erlang-style process identifier with 3 components. Node
        component is always 0 for local node, otherwise it can take arbitrary
        integer values.

        A pid is created by Erlang automatically. Pyrlang Process also generates
        one when it is born to register itself in the process registry. Pid
        uniquely identifies a running process in the cluster.
    """

    def __init__(self, node_name: str, id: int, serial: int,
                 creation: int) -> None:
        self.node_name_ = node_name
        """ NOTE: native encoder assumes this is a string. """

        self.id_ = id
        self.serial_ = serial
        self.creation_ = creation

    def is_local_to(self, node):
        return self.node_name_ == node.node_name_

    def __repr__(self) -> str:
        return "Pid<%d.%d.%d>@%s" % (self.creation_, self.id_, self.serial_,
                                     self.node_name_)

    def __str__(self) -> str:
        return self.__repr__()

    def equals(self, other) -> bool:
        return isinstance(other, Pid) \
               and self.node_name_ == other.node_name_ \
               and self.id_ == other.id_ \
               and self.serial_ == other.serial_ \
               and self.creation_ == other.creation_

    __eq__ = equals

    def __ne__(self, other):
        return not self.equals(other)

    def __hash__(self):
        return hash((PID_MARKER, self.node_name_,
                     self.id_, self.serial_, self.creation_))

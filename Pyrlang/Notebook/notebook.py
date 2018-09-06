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
from typing import Callable, Union, List

from Pyrlang.Term.atom import Atom
from Pyrlang.Term.pid import Pid
from Pyrlang.gen_server import GenServer
from Pyrlang.util import as_str


class Notebook(GenServer):
    """ Implements a flow for performing remote calculations from Erlang on
        Python node, while storing the results on Python side until they are
        ready. Each call result is stored in notebook history and a value index
        is returned to the caller. This index can be used as an argument
        in subsequent calls.
    """

    def __init__(self, options: dict, node_name: str):
        super().__init__(node_name, accepted_calls=["nb_call", "nb_retrieve"])

        self.history_ = dict()
        """ Recent calculation results indexed by integers or names. """

        self.history_ids_ = []
        """ Log of recent value ids in their creation order. Values, which were
            deleted because of history size limit, are also deleted here. """

        self.value_id_ = 1
        """ Next id to be used as result index. """

        self.history_limit_ = options.get("history", 50)
        """ History dict will be trimmed when its length is greater than this 
            limit to save memory. Attempt to refer to a trimmed value will 
            create value_not_found exception, also propagated to Erlang side. 
        """

    def nb_call(self, param: dict):
        """ Remote call from ``py.erl``: Calls function defined in ``args``,
            stores the result in history.

            :returns: Index for stored history value.
        """
        call_path = param[Atom("path")]
        call_args = param[Atom("args")]
        call_kwargs = param[Atom("kwargs")]
        fn = resolve_path(_prepare_path(call_path))
        result = fn(*call_args, **call_kwargs)

        index = self.value_id_
        self.value_id_ += 1
        self.history_[index] = result
        self.history_ids_.append(index)

        return Atom('ok'), index

    def nb_retrieve(self, value_id):
        """ Remote call from ``py.erl``: Retrieves a historical value by index.
        """
        if value_id in self.history_:
            return Atom('ok'), self.history_[value_id]

        return Atom('error'), Atom('not_found')


def new_context(node_name: str, options: dict) -> Pid:
    """ Create a new remote-call notebook context. Node_name argument will be
        automatically prepended to args by Rex.
    """
    nb = Notebook(options=options, node_name=node_name)
    return nb.pid_


def resolve_path(p: List[str]) -> Callable:
    """ Imports p[0] and then follows the list p, applying getattr repeatedly. """
    path = _prepare_path(p)

    val = __import__(as_str(path[0]))
    for item in path[1:]:
        val = getattr(val, as_str(item))

    return val


def _prepare_path(p: Union[list, str]) -> List[str]:
    if isinstance(p, str):
        return [p]
    return p

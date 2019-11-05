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
from typing import Callable, List, Tuple, Dict

from pyrlang.gen_server import GenServer
from pyrlang.util import as_str
from term.atom import Atom
from term.pid import Pid

LOG = logging.getLogger("pyrlang.notebook")


class Notebook(GenServer):
    """ Implements a flow for performing remote calculations from Erlang on
        Python node, while storing the results on Python side until they are
        ready. Each call result is stored in notebook history and a value index
        is returned to the caller. This index can be used as an argument
        in subsequent calls.
    """

    def __init__(self, options: dict):
        calls = ["nb_call",
                 "nb_retrieve",
                 "nb_batch",
                 "exit"  # allow Process.exit to be called remotely
                 ]
        super().__init__(accepted_calls=calls)

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

            :param param: contains ``path``: list of strings where first one is
                to be imported and remaining are used to find the function;
                ``args``: list of arguments for the callable; ``kwargs``;
                ``immediate``: will return the value instead of the value ref
                if this is ``True``, also will not update the history.
            :returns: Index for stored history value.
        """
        call_path = param[Atom("path")]
        call_args = self._resolve_valuerefs_in_args(param[Atom("args")])
        call_kwargs = self._resolve_valuerefs_in_kwargs(param[Atom("kwargs")])
        call_imm = param[Atom("immediate")]

        fn = self._resolve_path(call_path)
        result = fn(*call_args, **call_kwargs)

        if call_imm:
            return Atom('value'), result

        index = self._store_result(result)
        return Atom('ok'), result.__class__.__name__, index

    def nb_batch(self, batch: List[tuple], param: Dict[Atom, any]):
        """ Take a remote call from Erlang to execute batch of Python calls. """
        if not batch:
            return Atom("error"), Atom("batch_empty")

        call_imm = param[Atom("immediate")]
        for bitem in batch:
            call_path = bitem[Atom("path")]
            call_args = self._resolve_valuerefs_in_args(bitem[Atom("args")])
            call_kwargs = self._resolve_valuerefs_in_kwargs(bitem[Atom("kwargs")])
            call_ret = bitem[Atom("ret")]

            fn = self._resolve_path(call_path)
            result = fn(*call_args, **call_kwargs)

            last_result_name = self._store_result_as(result, call_ret)

        if call_imm:
            return Atom("value"), result

        return Atom("ok"), result.__class__.__name__, last_result_name

    def _store_result(self, result):
        """ Store result as a new numbered value. Trim overflowing values over
            the ``history_limit_``.
        """
        index = self.value_id_
        self.value_id_ += 1
        self.history_[index] = result
        self.history_ids_.append(index)
        self._maybe_trim_history()
        return index

    def _store_result_as(self, result, store_key):
        """ Store result as a new named value. Trim overflowing values over
            the ``history_limit_``.
        """
        LOG.debug("Store as %s => %s", store_key, result)
        self.history_[store_key] = result
        self.history_ids_.append(store_key)
        self._maybe_trim_history()
        return store_key

    def _maybe_trim_history(self):
        if len(self.history_ids_) <= self.history_limit_:
            return

        # Trim the history by deleting extra items in history_ids_
        overflow = len(self.history_ids_) - self.history_limit_
        trim_ids = self.history_ids_[0:overflow]

        for i in trim_ids:
            del self.history_[i]

        del self.history_ids_[0:overflow]

    def nb_retrieve(self, value_id):
        """ Remote call from ``py.erl``: Retrieves a historical value by index.
        """
        if value_id in self.history_:
            return Atom('ok'), self.history_[value_id]

        return Atom('error'), Atom('not_found')

    def _resolve_path(self, p: List[str]) -> Callable:
        """ Imports p[0] and then follows the list p, by applying getattr()
            repeatedly. """
        if isinstance(p, str):
            p = [p]

        # First element would be the import, or a stored value reference
        first_path_element = p[0]
        if isinstance(first_path_element, tuple) \
                and first_path_element[0] == Atom("$pyrlangval"):
            # First element is {'$pyrlangval', X} - query the value
            val = self._retrieve_value(first_path_element)
        else:
            # First element is a string, import it
            val = __import__(as_str(first_path_element))

        # Follow the elements in path, and getattr deeper
        for item in p[1:]:
            val = getattr(val, as_str(item))

        return val

    def _retrieve_value(self, pyrlang_val: Tuple[Atom, any]):
        k = pyrlang_val[1]

        if k not in self.history_:
            LOG.error("Value id %s not found in history (keys %s)",
                      k, self.history_.keys())

        return self.history_[k]

    def _resolve_valuerefs_in_args(self, args: list):
        """ For list of args, find pyrlangval references and resolve them from
            the history dict.
        """
        def resolve_arg(pyrlangval_tuple):
            if isinstance(pyrlangval_tuple, tuple) \
                    and pyrlangval_tuple[0] == Atom("$pyrlangval"):
                return self._retrieve_value(pyrlangval_tuple)

            return pyrlangval_tuple

        return list(map(resolve_arg, args))

    def _resolve_valuerefs_in_kwargs(self, dct: dict):
        """ For a dict of args, find pyrlangval references in dict values and
            resolve them from the history dict.
        """
        def resolve_arg(key_val):
            key, pyrlangval_tuple = key_val
            if isinstance(pyrlangval_tuple, tuple) \
                    and pyrlangval_tuple[0] == Atom("$pyrlangval"):
                return key, self._retrieve_value(pyrlangval_tuple)

            return key, pyrlangval_tuple

        return dict(map(resolve_arg, dct.items()))


def new_context(options: dict) -> Pid:
    """ Create a new remote-call notebook context. Node_name argument will be
        automatically prepended to args by Rex.
    """
    nb = Notebook(options=options)
    return nb.pid_


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
import traceback

from pyrlang2.gen_server import GS, call, cast, info
from term.atom import Atom

LOG = logging.getLogger(__name__)


class Rex(GS):
    def __init__(self):
        super().__init__()
        self.get_node().register_name(self, Atom('rex'))

    @call(1, lambda msg: True)
    def handle_call(self, msg):
        if type(msg) != tuple or len(msg) != 5 or msg[0] != Atom('call'):
            LOG.error("rex unknown call msg: %s", msg)
            return
        return act_on_msg(msg)

    @cast(1, lambda msg: True)
    def handle_cast(self, msg):
        if type(msg) != tuple or len(msg) != 5 or msg[0] != Atom('cast'):
            LOG.error("rex unknown cast msg: %s", msg)
            return
        act_on_msg(msg)

    @info(1, lambda msg: True)
    def handle_info(self, msg):
        LOG.error("rex unhandled info msg: %s", msg)


def act_on_msg(msg):
    mod = msg[1]
    fun = msg[2]
    args = msg[3]
    return execute(mod, fun, args)


def execute(mod, fun, args, traceback_depth=5):
    try:
        pmod = __import__(mod)
        pfun = getattr(pmod, fun)
        return pfun(*args)
    except Exception as e:
        if traceback_depth > 0:
            e.traceback = traceback.format_exc(traceback_depth)
        LOG.exception("got and exception when executing RPC call %s.%s(*%s)",
                      mod, fun, args)
        return e

__all__ = ['Rex']

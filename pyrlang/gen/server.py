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

""" Example of how to inherit from GenServer:

.. code-block:: python

    class MyProcess(GenServer):
        def __init__(self, node) -> None:
            GenServer.__init__(self, node, accepted_calls=['hello'])

        def hello(self):
            return self.pid_
"""
import asyncio


from pyrlang.process import Process
from pyrlang.match import Match
from term import Atom

import logging
LOG = logging.getLogger(__name__)


def _atom_match_factory(atom: Atom):
    def simple_match(msg):
        if type(msg) == tuple and len(msg) > 0 and msg[0] == atom:
            return True
        else:
            return False

    return simple_match


class GSM(type):
    """
    Meta class for GenServer

    Looks for functions that have been decorated and adds them to
    match functions
    """
    def __init__(cls, name, bases, nmspc):
        super().__init__(name, bases, nmspc)
        p_list = [(lambda x: False, lambda x: None)]
        cls._call_match = Match(p_list)
        cls._cast_match = Match(p_list)
        cls._info_match = Match(p_list)
        patterns = {
            'call': [],
            'cast': [],
            'info': [],
        }
        for attr in nmspc.values():
            cls._maybe_add_pattern(attr, patterns)

        for handler_type, type_patterns in patterns.items():
            cls._finalize_match(handler_type, type_patterns)

    def _finalize_match(cls, handler_type, type_patterns):
        if not type_patterns:
            return

        attr_name = "_{}_match".format(handler_type)
        type_patterns.sort(key=lambda tp: tp[0])
        type_patterns = [tp[1] for tp in type_patterns]
        m = Match(type_patterns)
        setattr(cls, attr_name, m)

    @staticmethod
    def _maybe_add_pattern(attr, patterns):
        """
        add functions pattern to res (no return but side effects)
        :param attr: attr that might be added
        :param patterns: the dict of lists that we add patterns to
        :return: None
        """
        handler_type = getattr(attr, '_gen_handler', False)

        if not handler_type:
            return
        if handler_type not in ['call', 'cast', 'info']:
            raise AttributeError("unknown handler type {}".format(handler_type))

        o = attr._gen_order
        p = attr._gen_pattern
        LOG.debug("adding {} {} with pattern {}".format(handler_type,
                                                        attr,
                                                        p))
        patterns[handler_type].append((o, p))


class GenServer(Process, metaclass=GSM):
    def __init__(self):
        self.state = 'init'
        super().__init__()

        self.__timeout_coro = None

        call_match = _atom_match_factory(Atom('$gen_call'))
        cast_match = _atom_match_factory(Atom('$gen_cast'))
        self._match = Match([(call_match, self._pre_handle_call),
                             (cast_match, self._pre_handle_cast),
                             (lambda x: True, self._pre_handle_info)])

    def _timeout(self):
        """Internal function see `timeout`"""
        self.inbox_.put_nowait(Atom('timeout'))

    def timeout(self, seconds=None):
        """
        set the gen_server timeout, will generate inbox `Atom('timeout')`
        :param seconds:
        :return:
        """
        if self.__timeout_coro:
            self.__timeout_coro.cancel()
        if seconds is None:
            return
        e = self.get_node().get_loop()
        self.__timeout_coro = e.call_later(seconds, self._timeout)

    async def process_loop(self):
        """ Polls inbox in an endless loop.
            .. note::

                This will not be executed if the process was constructed with
                ``passive=True`` (the default). Passive processes should read
                their inbox directly from ``self.inbox_``.
        """
        self.state = 'running'
        while not self.is_exiting_:
            # If any messages have been handled recently, do not sleep
            # Else if no messages, sleep for some short time
            LOG.debug("jso: waiting for inbox data")
            msg = await self.receive()
            if type(msg) != tuple or len(msg) != 2:
                LOG.debug("Got unhandled msg: %s", msg)
                continue
            LOG.debug("got msg =  %s", msg)
            fun, args = msg
            # todo: make this if thing in meta class so that we only evaluate
            #       this once
            import asyncio
            if asyncio.iscoroutinefunction(fun):
                # since fun will be returned by a function that only have access
                # to the Class rather than the instance, we have to send `self`
                # as an argument
                await fun(self, args)
            else:
                fun(self, args)

        LOG.debug("Process %s process_loop stopped", self.pid_)
        self.state = 'exiting'

    def _pre_handle_call(self, msg):
        p = self._call_match(msg[2])
        if not p:
            LOG.debug("no matched call function found for %s", self)
            return
        return p.run(msg)

    def _pre_handle_cast(self, msg):
        p = self._cast_match(msg[1])
        if not p:
            return
        return p.run(msg)

    def _pre_handle_info(self, msg):
        p = self._info_match(msg)
        if not p:
            return
        return p.run(msg)


class GenServerInterface(object):
    """
    Class that implements an interface for gen_servers

    This class is intended to be used in `Process` instances where
    gen_server behaviour interaction is necessary.

    in a raw form this class is initiated with the calling process instance and
    the pid of the destination process, then you can make calls and cast:

        gsi = GenServerInterface(some_process, remote_pid)
        await gsi.call(call_request)
        await gsi.cast(cast_request)
        gsi.cast_nowait(other_cast_request)

    If you want to create an interface you override this class and implement
    the functions, hiding the gen_server methods, just like in erlang
    """
    def __init__(self, calling_process: Process, destination_pid):
        self._calling_process = calling_process
        self._destination_pid = destination_pid
        self._node = calling_process.get_node()

    async def _do_call(self, label, request, timeout=5):
        calling_pid = self._calling_process.pid_
        m_ref = self._node.monitor_process(calling_pid,
                                           self._destination_pid)
        msg = (label, (calling_pid, m_ref), request)
        await self._node.send(calling_pid,
                              self._destination_pid,
                              msg)

        def pattern(in_msg):
            if type(in_msg) != tuple:
                return False
            if len(in_msg) != 2:
                return False
            if in_msg[0] != m_ref:
                return False
            return True

        match = Match([(pattern, lambda x: x[1])])
        res = await self._calling_process.receive(match, timeout)
        self._node.demonitor_process(calling_pid, self._destination_pid, m_ref)
        return res

    async def call(self, request, timeout=None):
        return await self._do_call(Atom('$gen_call'), request, timeout)

    async def cast(self, request):
        calling_pid = self._calling_process.pid_
        msg = (Atom('$gen_cast'), request)
        await self._node.send(calling_pid, self._destination_pid, msg)

    def cast_nowait(self, request):
        self._node.get_loop().create_task(self.cast(request))


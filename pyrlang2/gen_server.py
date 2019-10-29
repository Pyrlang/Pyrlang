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

import logging
import traceback
import asyncio

from typing import Union

from pyrlang2.process import Process
from pyrlang2 import gen
from pyrlang2.gen import GenIncomingMessage
from pyrlang2.util import as_str
from pyrlang2.match import Match, Pattern
from term import Atom

LOG = logging.getLogger("pyrlang.OTP")


class GenException(Exception):
    def __init__(self, msg, *args, **kwargs):
        LOG.error("GenException: %s", msg)
        Exception.__init__(self, msg, *args, **kwargs)


class GenServer(Process):
    """ Inherit from this instead of inheriting from the
        :py:class:`~Pyrlang.process.Process` to gain the ability to convert
        incoming ``gen:call`` messages into regular Python method calls. """
    def __init__(self, accepted_calls: Union[list, None] = None):
        """
        :param accepted_calls: None or list of strings, defines allowed names
            which will be converted into method calls on ``self``. A call name
            is first element of the tuple (atom, binary or ASCII string).
        """
        super().__init__()

        self.traceback_depth_ = 10

        if accepted_calls is None:
            accepted_calls = []
        self.gen_accepted_calls_ = {k: True for k in accepted_calls}
        """ List of strings with allowed messages which will be converted into 
            method calls. A incoming call is identified by its first element 
            (which must be atom, binary or string). 
        """

    @staticmethod
    def handle_info(msg):
        """ Similar to Erlang/OTP - handler receives all messages which were
            not recognized by gen message parser.
        """
        LOG.info("Info message %s", msg)

    def handle_one_inbox_message(self, msg):
        """ Function contains secret sauce - the ``gen:call`` parsing logic. """
        sys_msg = gen.parse_gen_message(msg, node_name=self.node_name_)

        if isinstance(sys_msg, str):
            return self.handle_info(msg)

        elif isinstance(sys_msg, GenIncomingMessage):
            LOG.debug("In call %s", sys_msg)
            self._handle_incoming_call(sys_msg)

        else:
            LOG.info("Unhandled sys message %s", sys_msg)

    def _handle_incoming_call(self, im: GenIncomingMessage):
        # TODO: noreply, and other stop codes
        call_msg = im.message_
        if isinstance(call_msg, tuple):
            f_name = as_str(call_msg[0])
            f_args = list(call_msg[1:])
        else:
            f_name = as_str(call_msg)
            f_args = []

        if f_name not in self.gen_accepted_calls_:
            raise GenException("Call to method %s is not in accepted_calls list"
                               % f_name)

        try:
            method = getattr(self, f_name)
            LOG.debug("method=%s", method)
            result = method(*f_args)
            LOG.debug("Replying with result=%s", result)
            im.reply(local_pid=self.pid_, result=result)

        except Exception as excpt:
            # Send an error
            if self.traceback_depth_ > 0:
                excpt.traceback = traceback.format_exc(self.traceback_depth_)

            im.reply_exit(local_pid=self.pid_, reason=excpt)


# Testing out a new interface for GenServer

def _atom_match_factory(atom: Atom):
    def simple_match(msg):
        if type(msg) == tuple and len(msg) > 0 and msg[0] == atom:
            return True
        else:
            return False

    return simple_match


def async_wrapper_factory(fun):
    async def async_wrapper(*args, **kwargs):
        return fun(*args, **kwargs)
    return async_wrapper


class HandleDecorator(object):
    """
    Base class for hadler functions decorator, don't use directly
    """
    handler_type = None

    def __init__(self, order, pattern=None):
        if not pattern:
            import warnings
            warnings.warn(DeprecationWarning("you should specify order"))
            pattern = order
            # high but not ensured highest this should be
            # removed soon anyway
            order = 999

        self.order = order
        self.pattern = pattern

    def __call__(self, fun):
        if not asyncio.iscoroutinefunction(fun):
            LOG.debug("function %s is a handle_%s and is not async",
                      fun,
                      self.handler_type)
            fun = async_wrapper_factory(fun)

        fun._gen_handler = self.handler_type
        run_fun = self._pattern_run_fun_factory(fun)
        fun._gen_pattern = Pattern(self.pattern, run_fun)
        fun._gen_order = self.order
        return fun

    @staticmethod
    def _pattern_run_fun_factory(fun):
        """
        Factory function for generating the run function of a pattern.

        Used by `HandleDecorator`
        :param fun: the function that should inject into the return
        :return: function that take msg in and return (fun, msg)
        """

        def _simple_pattern_run_fun(msg):
            return fun, msg

        return _simple_pattern_run_fun


def _handle_reply_wrapper(self, msg):
    """
    a wrapper function that
    :param self:
    :param msg:
    :return:
    """
    pass


class call(HandleDecorator):
    """
    handle_call decorator, decorate functions that should be part of the
    handle_call matching
    """
    handler_type = 'call'

    @staticmethod
    def _pattern_run_fun_factory(fun):

        async def _handle_reply_wrapper(gs_instance, msg):
            """
            wrapper function to strip a Atom('$gen_call') message to
            the actual message, then send a reply
            :param msg: 3 (Atom('$gen_call'), (sender, mref), message)
            :return: function that takes a message
            """
            LOG.critical("jso runfun %s, %s", fun, msg)
            (sender, mref) = msg[1]
            res = await fun(gs_instance, msg[2])
            LOG.critical("jso %s", gs_instance.__dict__)
            n = gs_instance.get_node()
            pid = gs_instance.pid_
            await n.send(pid, sender, (mref, res))

        return lambda msg: (_handle_reply_wrapper, msg)


class cast(HandleDecorator):
    """
    handle_cast decorator, decorate functions that should be part of the
    handle_cast matching
    """
    handler_type = 'cast'

    @staticmethod
    def _pattern_run_fun_factory(fun):
        """
        Factory function for generating the run function of a pattern.

        Used by `HandleDecorator`
        :param fun: the function that should inject into the return
        :return: function that take msg in and return (fun, msg)
        """

        def _simple_pattern_run_fun(msg):
            """
            we strip the Atom('gen_cast') from the message
            :param msg:
            :return:
            """
            return fun, msg[1]

        return _simple_pattern_run_fun


class info(HandleDecorator):
    """
    handle_info decorator, decorate functions that should be part of the
    handle_info matching
    """
    handler_type = 'info'


class GSM(type):
    """
    Meta class for GenServer

    Looks for functions that have been decorated and adds them to
    match functions
    """
    def __init__(cls, name, bases, nmspc):
        super().__init__(name, bases, nmspc)
        LOG.critical("cls = %s", cls)
        LOG.critical("\n\n\n")
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
        LOG.critical("adding {} {} with pattern {}".format(handler_type,
                                                           attr,
                                                           p))
        patterns[handler_type].append((o, p))


class GS(Process, metaclass=GSM):
    def __init__(self):
        self.state = 'init'
        super().__init__()

        call_match = _atom_match_factory(Atom('$gen_call'))
        cast_match = _atom_match_factory(Atom('$gen_cast'))
        self._match = Match([(call_match, self._pre_handle_call),
                             (cast_match, self._pre_handle_cast),
                             (lambda x: True, self._pre_handle_info)])

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
            LOG.critical("jso: waiting for inbox data")
            msg = await self.receive()
            if type(msg) != tuple or len(msg) != 2:
                LOG.critical("Got unhandled msg: %s", msg)
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
        LOG.critical("\n\n_pre_handle_call: %s\n", msg)
        p = self._call_match(msg[2])
        if not p:
            LOG.critical("no matched call function found")
            return
        return p.run(msg)

    def _pre_handle_cast(self, msg):
        LOG.critical("\n\n_pre_handle_cast: %s\n", msg)
        p = self._cast_match(msg[1])
        if not p:
            return
        return p.run(msg)

    def _pre_handle_info(self, msg):
        LOG.critical("\n\n_pre_handle_info: %s\n", msg)
        LOG.critical("jso: %s", self._info_match.__dict__)
        p = self._info_match(msg)
        if not p:
            return
        return p.run(msg)

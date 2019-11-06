# Copyright 2018-2019, Erlang Solutions Ltd, and S2HC Sweden AB
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
# limitations under the License.o
"""
Decorator funcitons for building Gen based classes.

the functions `call`, `cast`, `info` are function decorators.

If you need to overrdide for extra functionality you probably want to use
the classes `CallDecorator`, `CastDecorator`, `InfoDecorator` instead
"""

import asyncio

from pyrlang.match import Pattern

import logging
LOG = logging.getLogger(__name__)


def async_wrapper_factory(fun):
    async def async_wrapper(*args, **kwargs):
        return fun(*args, **kwargs)
    return async_wrapper


class HandleDecorator(object):
    """
    Base class for handler Decorators, don't use directly
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


class CallDecorator(HandleDecorator):
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
            (sender, mref) = msg[1]
            res = await fun(gs_instance, msg[2])
            n = gs_instance.get_node()
            pid = gs_instance.pid_
            await n.send(pid, sender, (mref, res))

        return lambda msg: (_handle_reply_wrapper, msg)


def call(*args, **kwargs):
    """wrapper function around the `CallDecorator` class"""
    return CallDecorator(*args, **kwargs)


class CastDecorator(HandleDecorator):
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


def cast(*args, **kwargs):
    """wrapper function around the `CastDecorator` class"""
    return CastDecorator(*args, **kwargs)


class InfoDecorator(HandleDecorator):
    """
    handle_info decorator, decorate functions that should be part of the
    handle_info matching
    """
    handler_type = 'info'


def info(*args, **kwargs):
    """wrapper function around the `InfoDecorator` class"""
    return InfoDecorator(*args, **kwargs)



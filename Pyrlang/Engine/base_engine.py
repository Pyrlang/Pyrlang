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

# import logging
from typing import Type

from Pyrlang.Engine.base_protocol import BaseProtocol


class BaseQueue:
    def put(self, v):
        raise NotImplementedError()

    def get(self):
        raise NotImplementedError()

    def is_empty(self):
        raise NotImplementedError()


class BaseEngine:
    """ This is base class for Pluggable Async Engines.
        This class abstracts away aspects handled by asyncio, gevent and similar
        event engines allowing different library to be selected.
    """

    def __init__(self):
        pass

    def sleep(self, seconds: float):
        """ Sleeps given amount of seconds in a nice compatible way. """
        raise NotImplementedError()

    def queue_new(self) -> BaseQueue:
        """ Creates a new Queue object compatible with the current async engine. """
        raise NotImplementedError()

    def connect_with(self, protocol_class: Type[BaseProtocol], host_port: tuple,
                     protocol_args: list, protocol_kwargs: dict):
        """ Connects to host_port with a given BaseProtocol handler.
        """
        raise NotImplementedError()

    def listen_with(self, protocol_class: Type[BaseProtocol], protocol_args: list,
                    protocol_kwargs: dict):
        """ Spawns a task which would listen on a random port with a given
            BaseProtocol handler
        """
        raise NotImplementedError()

    def socket_module(self):
        """ Returns current socket module (modified to be compatible with the
            current async library). Used functions are:
            *   socket.create_connection
            *   socket.error (Exception class)
            *   socket.gethostbyname
        """
        raise NotImplementedError()

    def spawn(self, a_callable):
        """ Spawns a task which will call loop_fn repeatedly while it
            returns False, else will stop. """
        raise NotImplementedError()

    def run_forever(self):
        """ Continues running event loop forever. """
        raise NotImplementedError()

    def call_later(self, t: float, fn):
        """ Schedules fn to be called after sleep(t). """
        raise NotImplementedError()

    def destroy(self):
        raise NotImplementedError()

import logging
from typing import Type

from Pyrlang.Engine.base_protocol import BaseProtocol


class BaseQueue:
    def put(self, v):
        raise NotImplementedError()

    def get(self):
        raise NotImplementedError()


class BaseEngine:
    """ This is base class for Pluggable Async Engines.
        This class abstracts away aspects handled by asyncio, gevent and similar
        event engines allowing different library to be selected.
    """

    def __init__(self):
        log_fmt = '%(asctime)-15s [%(name)s] %(module)s:%(lineno)i: %(message)s'
        logging.basicConfig(format=log_fmt)

    def sleep(self, seconds: float):
        raise NotImplementedError()

    def queue_new(self) -> BaseQueue:
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

    def socket_module():
        """ Returns current socket module (modified to be compatible with the
            current async library). Used functions are:
            *   socket.create_connection
            *   socket.error (Exception class)
            *   socket.gethostbyname
        """
        raise NotImplementedError()

    def spawn(self, a_callable):
        """ Creates an async task running together with other tasks on this
            async engine. I.e. a greenlet, a fiber, a thread, whatever there is
        """
        raise NotImplementedError()

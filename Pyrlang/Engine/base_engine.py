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

    def sleep(self, seconds: float):
        raise NotImplementedError()

    def queue_new(self) -> BaseQueue:
        raise NotImplementedError()

    def connect_with(self, protocol_class: Type[BaseProtocol], host_port: tuple,
                     protocol_args: list, protocol_kwargs: dict):
        raise NotImplementedError()

    def listen_with(self, protocol_class: Type[BaseProtocol], protocol_args: list,
                    protocol_kwargs: dict):
        pass

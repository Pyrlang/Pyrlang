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
import select
import socket
import traceback
from typing import Type

import asyncio
from asyncio import Queue, QueueEmpty

from pyrlang.Engine.base_engine import BaseEngine, BaseQueue
from pyrlang.Engine.base_protocol import BaseProtocol

LOG = logging.getLogger("Pyrlang")


class AsyncioQueue(BaseQueue):
    def __init__(self):
        self.q_ = Queue()

    def put(self, v):
        self.q_.put_nowait(v)

    def is_empty(self):
        return self.q_.empty()

    def get(self):
        """ Attempt to fetch one item from the queue, or return None. """
        try:
            return self.q_.get_nowait()
        except QueueEmpty:
            return None


class AsyncioEngine(BaseEngine):
    """ Compatibility driver for Asyncio.
        Create it before creating Node and pass as argument 'engine' like so:

        .. code-block:: python

            e = AsyncioEngine()
            node = Node(name="py@127.0.0.1", cookie="COOKIE", engine=e)
    """

    def __init__(self):
        super().__init__()
        self.loop_ = asyncio.get_event_loop()

    def sleep(self, seconds: float):
        self.loop_.run_until_complete(asyncio.sleep(seconds))

    def socket_module(self):
        return socket

    def queue_new(self) -> BaseQueue:
        """ Create Asyncio queue adapter """
        return AsyncioQueue()

    def connect_with(self, protocol_class: Type[BaseProtocol], host_port: tuple,
                     protocol_args: list, protocol_kwargs: dict
                     ) -> (BaseProtocol, socket.socket):
        """ Helper which creates a new connection and feeds the data stream into
            a protocol handler class.

            :rtype: tuple(protocol_class, gevent.socket)
            :param protocol_class: A handler class which has handler functions like
                    on_connected, consume, and on_connection_lost
            :param protocol_kwargs: Keyword args to pass to the handler constructor
            :param protocol_args: Args to pass to the handler constructor
            :param host_port: (host,port) tuple where to connect
        """
        LOG.info("Will connect to %s", host_port)
        sock = socket.create_connection(address=host_port)

        handler = protocol_class(*protocol_args, **protocol_kwargs)
        handler.on_connected(host_port)

        LOG.info("Connection to %s established", host_port)

        try:
            self.loop_.create_task(_read_loop(proto=handler,
                                              sock=sock,
                                              ev_loop=self.loop_))

        except Exception:
            LOG.error("Exception: %s", traceback.format_exc())

        return handler, sock

    def listen_with(self, protocol_class: Type[BaseProtocol],
                    protocol_args: list,
                    protocol_kwargs: dict):

        host_port = ('', 0)
        server_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_sock.bind(host_port)
        server_sock.listen(8)
        server_sock.setblocking(False)

        self.loop_.create_task(
            _accept_loop(server_sock=server_sock,
                         ev_loop=self.loop_,
                         read_loop_fn=_read_loop,
                         protocol_class=protocol_class,
                         protocol_args=protocol_args,
                         protocol_kwargs=protocol_kwargs))

        _, listening_port = server_sock.getsockname()
        LOG.info("Listening on %s (%d)", host_port, listening_port)
        return server_sock, listening_port

    def spawn(self, loop_fn):
        """ Spawns a task which will call loop_fn repeatedly while it
            returns False, else will stop. """
        self.loop_.create_task(_generic_async_loop(loop_fn=loop_fn))

    def run_forever(self):
        self.loop_.run_forever()

    def call_later(self, t: float, fn):
        self.loop_.create_task(_call_later_helper(t, fn))

    def destroy(self):
        self.loop_.stop()
        # self.loop_.close()

#
# Helpers for serving incoming connections and reading from the connected socket
#


async def _call_later_helper(t, fn):
    """ Sleeps T amount of seconds then calls a callable fn and dies. """
    await asyncio.sleep(t)
    fn()


async def _accept_loop(server_sock,
                       read_loop_fn,
                       ev_loop: asyncio.AbstractEventLoop,
                       protocol_class: Type[BaseProtocol],
                       protocol_args: list,
                       protocol_kwargs: dict):
    while True:
        client_sock, _addr = await ev_loop.sock_accept(server_sock)
        proto = protocol_class(*protocol_args, **protocol_kwargs)
        peer_name = client_sock.getpeername()
        LOG.debug("Incoming from %s", peer_name)
        proto.on_connected(peer_name)
        ev_loop.create_task(read_loop_fn(proto=proto,
                                         sock=client_sock,
                                         ev_loop=ev_loop))


async def _generic_async_loop(loop_fn):
    while loop_fn():
        await asyncio.sleep(0.01)


async def _read_loop(proto: BaseProtocol,
                     sock: socket.socket,
                     ev_loop: asyncio.AbstractEventLoop):
    collected = b''
    while True:
        if proto.close_requested_:
            return _disconnect(proto, sock, "Socket close requested")

        proto.periodic_check()
        try:
            s_read, s_write, s_error = select.select([sock], [sock], [sock], 0)
            if s_read:
                data = await ev_loop.sock_recv(sock, 4096)
                if not data:
                    return _disconnect(proto, sock, "Socket closed")

                collected += data

                # Try and consume repeatedly if multiple messages arrived
                # in the same packet
                while True:
                    collected1 = proto.on_incoming_data(collected)
                    if collected1 is None:
                        return _disconnect(
                            proto, sock, "Protocol requested to disconnect the socket")

                    if collected1 == collected:
                        break  # could not consume any more

                    collected = collected1

            elif s_write and len(proto.send_buffer_) > 0:
                await ev_loop.sock_sendall(sock, proto.send_buffer_)
                proto.send_buffer_ = b''

            # Check for errors too
            elif s_error:
                return _disconnect(proto, sock, "Error detected on the socket")

        except select.error as e:
            return _disconnect(proto, sock,
                               "Select error detected on the socket %s" % e)

        await asyncio.sleep(0.1)


def _disconnect(proto, sock, msg: str):
    LOG.error(msg)
    sock.close()
    proto.on_connection_lost()

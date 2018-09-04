import logging
import traceback
from typing import Type

import gevent
from gevent import socket, select
from gevent.queue import Queue, Empty
from gevent.server import StreamServer

from Pyrlang.Engine.base_engine import BaseEngine, BaseQueue
from Pyrlang.Engine.base_protocol import BaseProtocol

LOG = logging.getLogger("Pyrlang")


class GeventQueue(BaseQueue):
    def __init__(self):
        self.q_ = Queue()

    def put(self, v):
        self.q_.put(v)

    def is_empty(self):
        return self.q_.empty()

    def get(self):
        """ Attempt to fetch one item from the queue, or return None. """
        try:
            return self.q_.get(block=True, timeout=0.01)
        except Empty:
            return None


class GeventEngine(BaseEngine):
    """ Compatibility driver for Gevent.
        Create it before creating Node and pass as argument 'engine' like so:

        e = GeventEngine()
        node = Node(name="py@127.0.0.1", cookie="COOKIE", engine=e)
    """

    def __init__(self):
        super().__init__()
        from gevent import monkey
        monkey.patch_all()

    def sleep(self, seconds: float):
        gevent.sleep(seconds)

    def socket_module(self):
        return socket

    def queue_new(self) -> BaseQueue:
        """ Create Gevent queue adapter """
        return GeventQueue()

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
            g = gevent.spawn(_read_loop, proto=handler, sock=sock)
            g.start()

        except Exception as e:
            LOG.error("Exception: %s", traceback.format_exc())

        return handler, sock

    def listen_with(self, protocol_class: Type[BaseProtocol],
                    protocol_args: list,
                    protocol_kwargs: dict) -> (StreamServer, int):
        host_port = ('0.0.0.0', 0)
        srv_loop = make_serve_loop(protocol_class=protocol_class,
                                   protocol_args=protocol_args,
                                   protocol_kwargs=protocol_kwargs)
        in_srv = StreamServer(listener=host_port,
                              handle=srv_loop)
        in_srv.start()
        LOG.info("Listening on %s (%s)", host_port, in_srv.server_port)
        return in_srv, in_srv.server_port

    def spawn(self, loop_fn):
        """ Spawns a task which will call loop_fn repeatedly while it
            returns False, else will stop. """
        greenlet = gevent.spawn(lambda: _generic_gevent_loop(loop_fn))
        greenlet.start()

    def run_forever(self):
        while True:
            gevent.sleep(0.1)

#
# Helpers for serving incoming connections and reading from the connected socket
#


def _generic_gevent_loop(loop_fn):
    while loop_fn():
        gevent.sleep(0.01)


def make_serve_loop(protocol_class: Type[BaseProtocol],
                    protocol_args: list,
                    protocol_kwargs: dict):
    """ A basic connection handler that takes an accepted connection and feeds
        the data stream into the receiver protocol handler class.

        :param protocol_kwargs: Keyword args to pass to the handler class constructor
        :param protocol_args: Args to pass to the handler class constructor
        :type protocol_class: class
        :param protocol_class: A handler class which has handler functions like
                on_connected, consume, and on_connection_lost
        :return: A handler function suitable for passing to StreamServer
    """

    def _serve_loop(sock: socket.socket, address):
        LOG.info("Client %s connected", address)
        proto = protocol_class(*protocol_args, **protocol_kwargs)
        proto.on_connected(address)

        try:
            _read_loop(proto=proto, sock=sock)

        except Exception as e:
            LOG.error("Exception: %s", traceback.format_exc())

        finally:
            LOG.info("Client %s disconnected", address)
            sock.close()
            proto.on_connection_lost()

    return _serve_loop


def _read_loop(proto: BaseProtocol, sock: socket.socket):
    collected = b''
    while True:
        if len(proto.send_buffer_) > 0:
            sock.sendall(proto.send_buffer_)
            proto.send_buffer_ = b''

        proto.periodic_check()

        ready = select.select([sock], [], [], 0.001)
        try:
            if ready[0]:
                data = sock.recv(4096)
                # LOG.debug("data in: %s" % hex_bytes(data))

                collected += data

                # Try and consume repeatedly if multiple messages arrived
                # in the same packet
                while True:
                    collected1 = proto.on_incoming_data(collected)
                    if collected1 is None:
                        LOG.error("Protocol requested to disconnect the socket")
                        sock.close()
                        proto.on_connection_lost()
                        return

                    if collected1 == collected:
                        break  # could not consume any more

                    collected = collected1
            else:
                # HACK to keep idle CPU down to 0.3% while trying to maintain
                # lower latency
                select.select([sock], [], [], 0.1)
                # gevent.sleep(0.1)

        except select.error:
            # Disconnected probably or another error
            break

    sock.close()
    proto.on_connection_lost()

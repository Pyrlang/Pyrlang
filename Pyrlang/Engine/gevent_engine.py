import logging
import traceback
from typing import Type

import gevent
from gevent import socket, select
from gevent.queue import Queue, Empty
from gevent.server import StreamServer

from Pyrlang.Dist.util import hex_bytes
from Pyrlang.Engine.base_engine import BaseEngine, BaseQueue
from Pyrlang.Engine.base_protocol import BaseProtocol
from Pyrlang.Engine.task import Task

LOG = logging.getLogger("Pyrlang")


def task_loop_helper(t: Task):
    while t.task_loop():
        gevent.sleep(0.0)


class GeventQueue(BaseQueue):
    def __init__(self):
        self.q_ = Queue()

    def put(self, v):
        self.q_.put(v)

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

    @staticmethod
    def start_task(t: Task):
        """ Start task loop helper which will call periodically t.task_loop and
            sleep for 0
        """
        gevent.spawn(lambda: task_loop_helper(t))

    def queue_new(self) -> BaseQueue:
        """ Create Gevent queue which is asynchronously accessible. """
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
        handler.on_connected(sock, host_port)

        LOG.info("Connection to %s established", host_port)

        try:
            g = gevent.spawn(_read_loop,
                             handler=handler,
                             sock=sock)
            g.start()

        except Exception as e:
            LOG.error("Exception: %s", traceback.format_exc())

        return handler, sock

    def listen_with(self, protocol_class: Type[BaseProtocol],
                    protocol_args: list,
                    protocol_kwargs: dict) -> StreamServer:
        host_port = ('0.0.0.0', 0)
        srv_loop = make_serve_loop(protocol_class=protocol_class,
                                   protocol_args=protocol_args,
                                   protocol_kwargs=protocol_kwargs)
        in_srv = StreamServer(listener=host_port,
                              handle=srv_loop)
        in_srv.start()
        LOG.info("Listening on %s (%s)", host_port, in_srv.server_port)
        return in_srv

    def spawn(self, a_callable):
        greenlet = gevent.spawn(a_callable)
        greenlet.start()

#
# Helpers for serving incoming connections and reading from the connected socket
#


def make_serve_loop(protocol_class, protocol_args, protocol_kwargs):
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
        proto.on_connected(sock, address)

        try:
            _read_loop(proto, sock)

        except Exception as e:
            LOG.error("Exception: %s", traceback.format_exc())

        finally:
            LOG.info("Client %s disconnected", address)
            sock.close()
            proto.on_connection_lost()

    return _serve_loop


def _read_loop(handler: BaseProtocol, sock: socket.socket):
    collected = b''
    while True:
        # a full packet before calling on_packet in the handler class
        # Ideally instead of 10 msec timeout, this should have been
        # infinity (that is None), but post that change the messaging
        # stops working because it is possible that this would
        # block other actions, like handling messages on the queues.
        # TODO: find a better solution.
        # Additionally, anything lower than 10 msec would unnecessarily consume
        # CPU, because epoll_wait() will have a time out 1 msec if the timeout
        # here is set to 0.0, which is bad for idle python nodes.
        # Note that setting a value of 0.01 or 10 msec would imply that
        # python node would not be able to send replies faster than 10 msec
        # to Erlang node, but this is an acceptable delay condering the
        # idle python node cpu consumption issue mentioned above.

        # update: set lower timeout of 1 millisecond but do a select for
        # longer time 1 second further down below to save cpu cycles
        # when there are no messages.
        # This is a HACK
        ready = select.select([sock], [], [], 0.001)
        try:
            if ready[0]:
                data = sock.recv(4096)
                # LOG.debug("data in: %s" % hex_bytes(data))

                collected += data

                # Try and consume repeatedly if multiple messages arrived
                # in the same packet
                while True:
                    collected1 = handler.on_incoming_data(collected)
                    if collected1 is None:
                        LOG.error("Protocol requested to disconnect the socket")
                        sock.close()
                        handler.on_connection_lost()
                        return

                    if collected1 == collected:
                        break  # could not consume any more

                    collected = collected1
            else:
                # TODO: Call handle_inbox after some cycles of consume too!
                handler.handle_inbox()
                # HACK to keep idle CPU down to 0.3% while trying to maintain
                # lower latency
                select.select([sock], [], [], 0.1)
                # gevent.sleep(0.1)

        except select.error:
            # Disconnected probably or another error
            break

    sock.close()
    handler.on_connection_lost()

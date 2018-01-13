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

from __future__ import print_function
import traceback

import gevent
import gevent.select as select
from gevent import socket


def _handle_socket_read(handler, sock):
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
                # print("data in: %s" % hex_bytes(data))

                collected += data

                # Try and consume repeatedly if multiple messages arrived
                # in the same packet
                while True:
                    collected1 = handler.consume(collected)
                    if collected1 is None:
                        print("Protocol requested to disconnect the socket")
                        break
                    if collected1 == collected:
                        break  # could not consume any more

                    collected = collected1
            else:
                handler.handle_inbox()
                # HACK to keep idle CPU down to 0.3% while
                # trying to maintain lower latency
                select.select([sock], [], [], 1.0)

        except select.error:
            # Disconnected probably or another error
            break

    sock.close()
    handler.on_connection_lost()


def make_handler_in(receiver_class, args, kwargs):
    """ A basic connection handler that takes an accepted connection and feeds
        the data stream into the receiver protocol handler class.

        :param kwargs: Keyword args to pass to the handler class constructor
        :param args: Args to pass to the handler class constructor
        :type receiver_class: class
        :param receiver_class: A handler class which has handler functions like
                on_connected, consume, and on_connection_lost
        :return: A handler function suitable for passing to StreamServer
    """

    def _handle_connect_disconnect(sock, address):
        print("Client connected", address)

        receiver = receiver_class(*args, **kwargs)
        receiver.on_connected(sock, address)

        try:
            _handle_socket_read(receiver, sock)

        except Exception as e:
            print("\nException: %s" % e)
            traceback.print_exc()
            print()

        finally:
            print("Client disconnected", address)
            sock.close()
            receiver.on_connection_lost()

    return _handle_connect_disconnect


def connect_with(protocol_class, host_port: tuple,
                 args: list, kwargs: dict):
    """ Helper which creates a new connection and feeds the data stream into
        a protocol handler class.

        :rtype: tuple(protocol_class, gevent.socket)
        :type protocol_class: class
        :param protocol_class: A handler class which has handler functions like
                on_connected, consume, and on_connection_lost
        :param kwargs: Keyword args to pass to the handler class constructor
        :param args: Args to pass to the handler class constructor
        :param host_port: (host,port) tuple where to connect
    """

    sock = socket.create_connection(address=host_port)

    handler = protocol_class(*args, **kwargs)
    handler.on_connected(sock, host_port)

    print("Connection to %s established" % str(host_port))

    try:
        g = gevent.spawn(_handle_socket_read, handler, sock)
        g.start()

    except Exception as e:
        print("\nException: %s" % e)
        traceback.print_exc()
        print()

    return handler, sock


__all__ = ['make_handler_in', 'connect_with']

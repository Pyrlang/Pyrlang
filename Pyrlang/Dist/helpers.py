# Copyright 2017, Erlang Solutions Ltd.
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
import gevent.select as select
from gevent import socket


def _handle_socket_read(receiver, sock):
    collected = b''
    while True:
        # a full packet before calling on_packet in the handler class
        ready = select.select([sock], [], [], 0.0)
        try:
            if ready[0]:
                data = sock.recv(4096)
                # print("data in: %s" % hex_bytes(data))

                collected += data

                # Try and consume repeatedly if multiple messages arrived
                # in the same packet
                while True:
                    collected1 = receiver.consume(collected)
                    if collected1 is None:
                        print("Protocol requested to disconnect the socket")
                        break
                    if collected1 == collected:
                        break  # could not consume any more

                    collected = collected1
            else:
                while not receiver.inbox_.empty():
                    msg = receiver.inbox_.get_nowait()
                    receiver.handle_one_inbox_message(msg)
                    # Longer sleep when there's no data
        except select.error:
            # Disconnected probably or another error
            break


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
        _handle_socket_read(handler, sock)

    except Exception as e:
        print("\nException: %s" % e)
        traceback.print_exc()
        print()

    finally:
        print("Client disconnected", host_port)
        sock.close()
        handler.on_connection_lost()

    return handler, sock


__all__ = ['make_handler_in', 'connect_with']

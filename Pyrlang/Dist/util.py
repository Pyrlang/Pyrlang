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
import struct
import traceback

import gevent
import gevent.select as select


def u16(data: bytes, pos: int = 0):
    return struct.unpack(">H", data[pos:pos + 2])[0]


def u32(data: bytes, pos: int = 0):
    return struct.unpack(">I", data[pos:pos + 4])[0]


def i32(data: bytes, pos: int = 0):
    return struct.unpack(">i", data[pos:pos + 4])[0]


def to_u32(val):
    return struct.pack(">I", val)


def to_i32(val):
    return struct.pack(">i", val)


def to_u16(val):
    return struct.pack(">H", val)


def make_handler(receiver_class, args, kwargs):
    """ A basic connection handler that applies a receiver object to each
        connection.
    """

    def _handle_connect_disconnect(socket, address):
        print("Client connected", address)

        receiver = receiver_class(*args, **kwargs)
        receiver.on_connected(socket, address)

        try:
            _handle_socket_read(receiver, socket)

        except Exception as e:
            print("\nException: %s" % e)
            traceback.print_exc()
            print()

        finally:
            print("Client disconnected", address)
            socket.close()
            receiver.on_connection_lost()

    def _handle_socket_read(receiver, socket):
        collected = b''
        while True:
            # a full packet before calling on_packet in the handler class
            ready = select.select([socket], [], [], 0.0)
            try:
                if ready[0]:
                    data = socket.recv(4096)
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

    return _handle_connect_disconnect


def hex_bytes(data: bytes, sep: str= " "):
    """ Format a bytes() object as a hex dump """
    return sep.join("{:02x}".format(bval) for bval in data)


def dec_bytes(data: bytes, sep: str= " "):
    """ Format a bytes() object as a decimal dump """
    return sep.join(str(bval) for bval in data)


def schedule(delay, func, *args, **kw_args):
    """ Spawns a greenlet with args periodically """
    gevent.spawn_later(0, func, *args, **kw_args)
    gevent.spawn_later(delay, schedule, delay, func, *args, **kw_args)


__all__ = ['make_handler', 'hex_bytes', 'dec_bytes', 'schedule',
           'u16', 'u32', 'i32',
           'to_u16', 'to_u32', 'to_i32']

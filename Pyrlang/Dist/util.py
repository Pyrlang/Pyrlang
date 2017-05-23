from __future__ import print_function
import struct

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
            print("Client disconnected", address)

        except socket.Exception as e:
            print(e)

        finally:
            socket.close()
            receiver.on_connection_lost()

    def _handle_socket_read(receiver, socket):
        collected = b''
        while True:
            # a full packet before calling on_packet in the handler class
            ready = select.select([socket], [], [], 1)
            try:
                if ready[0]:
                    data = socket.recv(4096)
                    collected += data
                    collected = receiver.consume(collected)
                    if collected is None:
                        print("Protocol requested to disconnect the socket")
                        break
                    gevent.sleep(0.0)
                else:
                    while not receiver.inbox_.empty():
                        msg = receiver.inbox_.get_nowait()
                        receiver.handle_one_inbox_message(msg)
                    # Longer sleep when there's no data
                    gevent.sleep(0.05)
            except select.error:
                # Disconnected probably or another error
                break

    return _handle_connect_disconnect


def hex_bytes(s: bytes):
    return " ".join("{:02x}".format(c) for c in s)


# def schedule(delay, func, *args, **kw_args):
#     """ Runs a func with args periodically """
#     gevent.spawn_later(0, func, *args, **kw_args)
#     gevent.spawn_later(delay, schedule, delay, func, *args, **kw_args)


__all__ = ['make_handler', 'hex_bytes', 'schedule',
           'u16', 'u32', 'i32',
           'to_u16', 'to_u32', 'to_i32']

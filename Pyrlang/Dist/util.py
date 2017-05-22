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

    def handle_fun(socket, address):
        print("Client connected", address)

        receiver = receiver_class(*args, **kwargs)
        receiver.on_connected(socket, address)

        try:
            collected = b''
            while True:
                # Because data is grouped in packets, first we try and assemble
                # a full packet before calling on_packet in the handler class
                # data = socket.recv(4096)
                # if not data:
                #    break  # disconnected
                ready = select.select([socket], [], [], 0.1)
                if ready[0]:
                    data = socket.recv(4096)

                    # print('Received data from client:', data)
                    collected += data
                    collected = receiver.consume(collected)
                    if collected is None:
                        print("Protocol requested to disconnect the socket")
                        break
                else:
                    while not receiver.inbox_.empty():
                        msg = receiver.inbox_.get_nowait()
                        receiver.handle_one_inbox_message(msg)
                    gevent.sleep(0.0)

            print("Client disconnected", address)

        except socket.Exception as e:
            print(e)

        finally:
            socket.close()
            receiver.on_connection_lost()

    return handle_fun


def hex_bytes(s: bytes):
    return " ".join("{:02x}".format(c) for c in s)


__all__ = ['make_handler', 'hex_bytes',
           'u16', 'u32', 'i32',
           'to_u16', 'to_u32', 'to_i32']

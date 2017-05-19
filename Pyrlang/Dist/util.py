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
                data = socket.recv(4096)
                if not data:
                    break  # disconnected

                # print('Received data from client:', data)
                collected += data
                collected = receiver.consume(collected)
                if collected is None:
                    print("Protocol requested to disconnect the socket")
                    break

            print("Client disconnected", address)

        except socket.Exception as e:
            print(e)

        finally:
            socket.close()
            receiver.on_connection_lost()

    return handle_fun


def hex_bytes(s: bytes):
    return " ".join("{:02x}".format(c) for c in s)


__all__ = ['make_handler', 'hex_bytes']

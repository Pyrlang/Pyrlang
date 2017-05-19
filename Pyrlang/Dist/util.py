def make_handler(receiver_class):
    """ A basic connection handler that applies a receiver object to each
        connection.
    """
    def handle_fun(socket, address):
        print('Client (%s) connected', address)

        receiver = receiver_class()
        receiver.on_connected(socket, address)

        try:
            while True:
                data = socket.recv(2)
                if data == "":
                    break
                print('Received packet from client: %s', data)
                receiver.on_packet(data)

            print('Client (%s) disconnected.', address)

        except socket.Exception as e:
            print(e)

        finally:
            socket.close()
            receiver.on_connection_lost()

    return handle_fun


__all__ = ['make_handler']

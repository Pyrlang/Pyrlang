from typing import Union


class BaseProtocol:
    """ Defines abstract protocol handler class. Adapters from Gevent, Asyncio
        etc are created in corresponding modules and are plugged in when
        engine is selected.
    """
    def __init__(self, *_args, **_kwargs):
        pass

    def on_connected(self, host, sock_port):
        pass

    def on_incoming_data(self, data: bytes) -> Union[bytes, None]:
        """ Attempt to consume first part of data as a packet

            :param data: The accumulated data from the socket which we try to
                partially or fully consume
            :return: Unconsumed data, incomplete following packet maybe or
                nothing. Returning None requests to close the connection
        """
        pass

    def on_connection_lost(self):
        pass

    def handle_inbox(self):
        """ When no data is available on the socket, try and handle some data
            that is already collected. """
        pass

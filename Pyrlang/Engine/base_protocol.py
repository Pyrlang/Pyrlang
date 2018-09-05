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

from typing import Union


class BaseProtocol:
    """ Defines abstract protocol handler class. Adapters from Gevent, Asyncio
        etc are created in corresponding modules and are plugged in when
        engine is selected.
    """
    def __init__(self, *_args, **_kwargs):
        self.send_buffer_ = b''

    def on_connected(self, host_port: tuple):
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

    def periodic_check(self):
        """ Override this to do periodic checks on something. """
        pass

    def send(self, msg: bytes):
        self.send_buffer_ += msg

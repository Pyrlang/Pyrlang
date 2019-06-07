# Copyright 2018, Erlang Solutions Ltd, and S2HC Sweden AB
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

""" Example of how to inherit from GenServer:

.. code-block:: python

    class MyProcess(GenServer):
        def __init__(self, node) -> None:
            GenServer.__init__(self, node, accepted_calls=['hello'])

        def hello(self):
            return self.pid_
"""

import logging
import traceback
from typing import Union

from pyrlang2.process import Process
from pyrlang2 import gen
from pyrlang2.gen import GenIncomingMessage
from pyrlang2.util import as_str

LOG = logging.getLogger("pyrlang.OTP")


class GenException(Exception):
    def __init__(self, msg, *args, **kwargs):
        LOG.error("GenException: %s", msg)
        Exception.__init__(self, msg, *args, **kwargs)


class GenServer(Process):
    """ Inherit from this instead of inheriting from the
        :py:class:`~Pyrlang.process.Process` to gain the ability to convert
        incoming ``gen:call`` messages into regular Python method calls. """
    def __init__(self, node_name: str,
                 accepted_calls: Union[list, None] = None):
        """
        :param accepted_calls: None or list of strings, defines allowed names
            which will be converted into method calls on ``self``. A call name
            is first element of the tuple (atom, binary or ASCII string).
        """
        super().__init__(node_name=node_name)

        self.traceback_depth_ = 10

        if accepted_calls is None:
            accepted_calls = []
        self.gen_accepted_calls_ = {k: True for k in accepted_calls}
        """ List of strings with allowed messages which will be converted into 
            method calls. A incoming call is identified by its first element 
            (which must be atom, binary or string). 
        """

    @staticmethod
    def handle_info(msg):
        """ Similar to Erlang/OTP - handler receives all messages which were
            not recognized by gen message parser.
        """
        LOG.info("Info message %s", msg)

    def handle_one_inbox_message(self, msg):
        """ Function contains secret sauce - the ``gen:call`` parsing logic. """
        sys_msg = gen.parse_gen_message(msg, node_name=self.node_name_)

        if isinstance(sys_msg, str):
            return self.handle_info(msg)

        elif isinstance(sys_msg, GenIncomingMessage):
            LOG.info("In call %s", sys_msg)
            self._handle_incoming_call(sys_msg)

        else:
            LOG.info("Unhandled sys message %s", sys_msg)

    def _handle_incoming_call(self, im: GenIncomingMessage):
        # TODO: noreply, and other stop codes
        call_msg = im.message_
        if isinstance(call_msg, tuple):
            f_name = as_str(call_msg[0])
            f_args = list(call_msg[1:])
        else:
            f_name = as_str(call_msg)
            f_args = []

        if f_name not in self.gen_accepted_calls_:
            raise GenException("Call to method %s is not in accepted_calls list"
                               % f_name)

        try:
            method = getattr(self, f_name)
            LOG.debug("method=%s", method)
            result = method(*f_args)
            LOG.debug("Replying with result=%s", result)
            im.reply(local_pid=self.pid_, result=result)

        except Exception as excpt:
            # Send an error
            if self.traceback_depth_ > 0:
                excpt.traceback = traceback.format_exc(self.traceback_depth_)

            im.reply_exit(local_pid=self.pid_, reason=excpt)

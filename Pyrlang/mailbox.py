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
import queue

import gevent
from gevent.queue import Queue

from Pyrlang import logger

LOG = logger.nothing
ERROR = logger.tty


class Mailbox:
    """ Implements a wrapper around gevent.Queue which serves as a message
        inbox with selective receive feature.
    """

    def __init__(self):
        self.queue_ = Queue()

    def put(self, m: tuple):
        LOG("Mailbox.put", m)
        self.queue_.put(m)

    def get(self):
        """ Receives ANY message whatever is the first in the queue. Blocks the
            greenlet if the queue is empty.
        """
        return self.queue_.get()

    def get_nowait(self):
        """ Receives ANY message whatever is the first.

            :raises queue.Empty: If the queue is empty
        """
        return self.queue_.get_nowait()

    def receive_wait(self, filter_fn: callable):
        while True:
            LOG(self.queue_.queue)

            m = self.receive(filter_fn=filter_fn)
            if m is not None:
                return m

            LOG("receive wait...")
            gevent.sleep(3.0)

    def receive(self, filter_fn: callable):
        """ Apply filter repeatedly to messages in the inbox.

            :returns: Message, if the filter returned True, otherwise ``None``
                if no message matches or the mailbox was empty
        """
        if self.queue_.empty():
            return None

        # try every element in the queue, get it, check it, place it into the
        # queue end (NOTE: This will mix the messages breaking the order)
        try:
            for i in range(len(self.queue_)):
                m = self.queue_.get_nowait()

                if filter_fn(m):
                    LOG("Mailbox: match return", m)
                    return m

                self.queue_.put(m)

        except queue.Empty:
            pass

        return None


__all__ = ['Mailbox']

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

import logging

LOG = logging.getLogger("pyrlang")


class NodeException(Exception):
    def __init__(self, msg, *args, **kwargs):
        LOG.error("NodeException: %s", msg)
        Exception.__init__(self, msg, *args, **kwargs)


class ProcessNotFoundError(NodeException):
    def __init__(self, msg, *args, **kwargs):
        LOG.error("NoProcess: %s", msg)
        Exception.__init__(self, msg, *args, **kwargs)


class BadArgError(Exception):
    def __init__(self, msg, *args, **kwargs):
        LOG.error("Bad Argument: %s", msg)
        Exception.__init__(self, msg, *args, **kwargs)

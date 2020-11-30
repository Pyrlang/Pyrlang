# Copyright 2018-2020, Erlang Solutions Ltd, and S2HC Sweden AB
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
from pyrlang import node_db

import logging
LOG = logging.getLogger(__name__)


def check_state():
    """
    introspect the running node and return information about it
    :return:
    """
    n = node_db.get()
    return {
        'pid_count': n.pid_counter_,
        'processes': get_processes(n),
        'reg_names': get_registered_names(n)
    }


def get_processes(node=None):
    if not node:
        node = node_db.get()
    return list(node.processes_.keys())


def get_registered_names(node=None):
    if not node:
        node = node_db.get()
    return list(node.reg_names_.items())


def check_process(pid):
    n = node_db.get()
    pid = n.where_is(pid)
    p = n.processes_[pid]
    ret = p.process_info()
    ret['state'] = p.__dict__
    LOG.info("monitor request, check_process:\n\n%s\n", ret)
    return dict((k, str(v)) for k, v in ret.items())

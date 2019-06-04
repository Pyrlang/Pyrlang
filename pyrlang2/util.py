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


def start_pyrlang():
    """
    This is invoked on ``import Pyrlang``. Function checks OS environment
    variables documented at :doc:`configuration`.
    """
    import os
    level = os.getenv("PYRLANG_LOG_LEVEL", "")
    if level and level in ['CRITICAL', 'ERROR', 'WARNING', 'INFO', 'DEBUG',
                           'NOTSET']:
        the_logger = logging.getLogger("pyrlang")
        the_logger.setLevel(getattr(logging, level))

    if os.getenv("PYRLANG_ENABLE_LOG_FORMAT", "no").upper() in ["1", "yes", "true", "on"]:
        log_fmt = '%(asctime)-15s [%(name)s] %(module)s:%(lineno)i: %(message)s'
        logging.basicConfig(format=log_fmt)

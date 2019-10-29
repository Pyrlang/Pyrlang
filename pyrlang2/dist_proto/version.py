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

""" Implement shared pieces of Erlang node negotiation and dist_proto
    protocol
"""

DIST_VSN = 5
DIST_VSN_PAIR = (DIST_VSN, DIST_VSN)
" Supported dist_proto protocol version (MAX,MIN). "


def dist_version_check(max_min: tuple) -> bool:
    """ Check pair of versions against version which is supported by us

        :type max_min: tuple(int, int)
        :param max_min: (Max, Min) version pair for peer-supported dist version
    """
    return max_min[0] >= DIST_VSN >= max_min[1]


def check_valid_dist_version(max_min: tuple) -> bool:
    """
    Check that the version is supported
    :param max_min: tuple(int, int
    :return: True if ok
    """
    return max_min[0] <= DIST_VSN <= max_min[1]

# __all__ = ['DIST_VSN', 'DIST_VSN_PAIR']

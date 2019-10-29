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


def _make_pattern(p):
    if isinstance(p, Pattern):
        return p
    return Pattern(*p)


def _simple_match(data):
    return True


def _simple_run(data):
    return data


class Match(object):
    """
    Match class to mimic an aid in the erlang pattern matching used in calls
    and, in our case more importantly, in receive
    """
    def __init__(self, patterns=None):
        if not patterns:
            # make catch all that returns raw data
            patterns = [(None, None)]
        self._patterns = [_make_pattern(p) for p in patterns]

    def __call__(self, data):
        for p in self._patterns:
            if not p.match(data):
                continue
            return p
        return False

    def match(self, data):
        return self(data)


class Pattern(object):
    def __init__(self, match_fun=None, run_fun=None):
        if not match_fun:
            match_fun = _simple_match

        if not run_fun:
            run_fun = _simple_run

        if not callable(match_fun):
            raise AttributeError("match fun {} not callable".format(match_fun))
        if not callable(run_fun):
            raise AttributeError("run fun {} not callable".format(run_fun))
        self.__match_fun = match_fun
        self.__run_fun = run_fun

    def match(self, data):
        return self.__match_fun(data)

    def run(self, data):
        return self.__run_fun(data)


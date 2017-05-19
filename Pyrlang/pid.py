from __future__ import print_function

PID_MARKER = "pyrlang.Pid"


class ErlPid:
    """ Process identifier implementation
    """

    def __init__(self, p) -> None:
        self.pid = p

    def equals(self, other) -> bool:
        return isinstance(other, ErlPid) and self.pid == other.pid

    __eq__ = equals

    def __ne__(self, other):
        return not self.equals(other)

    def __hash__(self):
        return hash((PID_MARKER, self.pid))

__all__ = ['ErlPid']

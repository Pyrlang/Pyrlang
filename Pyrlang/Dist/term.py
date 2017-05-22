from __future__ import print_function
from builtins import chr


class Atom:
    def __repr__(self) -> str:
        return "Atom'%s'" % self.text_

    def __init__(self, text: str) -> None:
        self.text_ = text


class List:
    """ Erlang list which stores elements in a Python list, can have a tail
        for improper list representation, and can be interpreted as Python
        string optionally
    """
    def __repr__(self) -> str:
        return "List[%s | %s]" % (self.elements_, self.tail_)

    def __init__(self) -> None:
        self.elements_ = []
        self.tail_ = []

    def __str__(self) -> str:
        return self.__repr__()

    def append(self, x):
        self.elements_.append(x)

    def set_tail(self, t):
        self.tail_ = t

    def as_unicode(self):
        return "".join([chr(x) for x in self.elements_])


class Pid:
    def __init__(self, node, id, serial, creation) -> None:
        self.node_ = node
        self.id_ = id
        self.serial_ = serial
        self.creation_ = creation

    def __repr__(self) -> str:
        return "Pid<%d.%d.%d>@%s" % (self.id_, self.serial_, self.creation_,
                                     self.node_.text_)

    def __str__(self) -> str:
        return self.__repr__()


class Reference:
    def __init__(self, node, creation, id) -> None:
        self.node_ = node
        self.id_ = id
        self.creation_ = creation

    def __repr__(self) -> str:
        return "Ref<%d %s>@%s" % (self.creation_, self.id_, self.node_.text_)

    def __str__(self) -> str:
        return self.__repr__()

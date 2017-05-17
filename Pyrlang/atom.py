from __future__ import print_function
from future.utils import python_2_unicode_compatible

ATOM_MARKER = "pyrlang.Atom"


@python_2_unicode_compatible
class ErlAtom:
    def __init__(self, t: str) -> None:
        self.text = t

    def equals(self, other) -> bool:
        return isinstance(other, ErlAtom) and self.text == other.text

    __eq__ = equals

    def __ne__(self, other):
        return not self.equals(other)

    def __hash__(self):
        return hash((ATOM_MARKER, self.text))

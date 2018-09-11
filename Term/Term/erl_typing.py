from Term import atom as a
from Term import pid as p
from Term import reference as r
from Term import fun as f

from typing import Union, List, Any

AnyTerm = Union[str, List[Any], tuple, dict, int, float, bytes,
                a.Atom, p.Pid, r.Reference, f.Fun]

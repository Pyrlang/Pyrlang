from Pyrlang.Term import atom as a
from Pyrlang.Term import pid as p
from Pyrlang.Term import reference as r
from Pyrlang.Term import fun as f

from typing import Union, List, Any

AnyTerm = Union[str, List[Any], tuple, dict, int, float, bytes,
                a.Atom, p.Pid, r.Reference, f.Fun]

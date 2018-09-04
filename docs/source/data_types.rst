Data Types in Pyrlang
=====================

Lists
-----

Erlang lists can be of 2 kinds:

*   Regular lists of anything:

    *   Unicode strings, which are regular lists of integers in unicode range.
    *   8-bit strings, sometimes called latin-1 or ASCII, which are regular lists
        of bytes.

* Improper lists (those with last cell's tail being not ``[] NIL``).

Pyrlang always decodes incoming regular lists as Python lists,
use helper functions in ``Pyrlang.Term.list`` module to extract strings.
If incoming string contained only bytes (integers between 0 and 255) then
Erlang node will optimize the encoding and send a byte array. In this case you
will receive Python ``bytes`` string, and not a list, which is
**the same as if you have sent a binary**, so to reduce confusion always send
strings as UTF8 binaries.

A regular Erlang list always has an invisible ``[]`` (``NIL``) set as tail of
its last cons cell. Regular lists map directly to Python lists and
**there is no easy way to tell a list of integers from a string** other than
check elements ranges and assume that is a printable string. Unless you received
Python ``bytes`` then it's easy.

An improper Erlang list has some other value than ``[] NIL`` as the tail of
its last cell.
Pyrlang returns these as Python tuple ``(list, tail)``.
To tell Pyrlang encoder to send an improper list back to Erlang, use the
:py:class:`~Pyrlang.Term.list.ImproperList` class.


Binaries
--------

Pyrlang always decodes incoming Erlang binaries into Python ``bytes`` objects.

Bitstrings are decoded as Python pairs of ``(bytes, last_byte_bits:int)``
To be able to send a bitstring back to Erlang, use class
:py:class:`~Pyrlang.Term.bitstring.BitString`.

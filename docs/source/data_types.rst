Data Types in Pyrlang
=====================

Lists
-----

Erlang lists can be of 2 kinds:

* Regular lists of anything:
    * Unicode strings, which are regular lists of integers in unicode range
    * 8-bit strings, sometimes called latin-1 or ASCII, which are regular lists
        of bytes.
* Improper lists.

A regular list always has an invisible ``[]`` (``NIL``) set as tail of its last
cons cell. Regular lists map directly to Python lists or strings and
**there is no easy way to tell a list of integers from a string** other than
check elements ranges and assume that is a printable string.

An improper list has some other value than ``[] NIL`` as its last cell tail.
For Pyrlang these are represented by an ``ImproperList`` object.


Binaries
--------

Pyrlang always converts a simple Erlang binary to Python ``bytes`` object.

Bitstrings are accepted and returned as Python tuple ``(bytes, last_byte_bits:int)``

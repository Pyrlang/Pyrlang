Pyrlang
=======

This is a drop-in Erlang node implementation in Python, designed to allow
access to existing Python projects from Erlang and the opposite. 
With just a few lines of startup code your Python program becomes a fully
functional Erlang node.
 
 
Documentation
-------------

http://pyrlang.readthedocs.io/en/latest/


Features
--------

*   Based on gevent which supports Python 2 and 3;
*   Erlang distribution protocol
*   Registry of Python 'processes', which are gevent Greenlets and have a pid
    and an optional registered name;
*   Send and receive messages locally and remotely by pid or name;
*   Supports `net_adm` pings
*   Supports RPC calls. An RPC call can propagate an exception from 
    Python to Erlang;
*   Can create simple process-like Python objects, with a helper to parse
    gen_server-style calls


| Erlang               | Python                      | Notes                                                                                 |
|----------------------|-----------------------------|---------------------------------------------------------------------------------------|
| Integer, big integer | Python integer              | Python is capable if big integers too                                                 |
| Float                | Python float                |                                                                                       |
| String               | term.List                   | Has a method `as_unicode()` to get the string                                         |
| Atom                 | term.Atom or string         | Can use str() or access text_ field directly. Can decode both UTF8 and Latin-1 atoms. |
| Regular Erlang list  | Python list                 | Use helper functions in `Term.list` module to convert to string                       |
| Improper Erlang list | (list, AnyTerm)             | A tuple with list and the tail element of the improper list                           |
|                      |                             | Returning an improper list to Erlang requires use of `ImproperList` helper object     |         
| Tuple                | Python tuple                |                                                                                       |
| Map                  | Python dict                 |                                                                                       |
| Binary               | Python bytes                |                                                                                       |
| Bitstring            | (bytes, int)                | A tuple of bytes and `last_byte_bits:int` defining incomplete last byte               |
|                      |                             | Returning a bitstring to Erlang requires use of BitString object                      |
| Pid, reference       | term.Pid and term.Reference | Always long external Pids and Refs with a node name in them                           |
| Lambda (fun)         | term.Fun                    | A class which holds parsed fun fields, not usable or useful in Python                 |
|                      | Any other object            | Any unknown Python object will be encoded as {'Classname', #{field1 => value1...}}    |


Building
--------

Install requirements via: `pip3 install -r requirements.txt`, this will also
pull requirements for Sphinx documentation generator (not needed if you only
use the library).

Requires `gevent` and `greenlet` (dependency of `gevent`) libraries which 
run on both Python 2 and 3. Running `make` will run a simple test which starts
a node at `py@127.0.0.1`.

Source for the documentation is in the `docs/` directory. It uses `sphinx`
documentation generator. In `docs/` run `make html` or just `make`.

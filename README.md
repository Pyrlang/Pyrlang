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
*   Erlang distribution protocol:
    *   Can encode lists, tuples, atoms, integers, floats, maps, 
        strings, binaries, pids, referenes, Python objects etc.;
    *   Can not encode functions, ports
*   Registry of Python 'processes', which are gevent Greenlets and have a pid
    and an optional registered name;
*   Send and receive messages locally and remotely by pid or name;
*   Supports `net_adm` pings
*   Supports RPC calls. An RPC call can propagate an exception from 
    Python to Erlang;
*   Can create simple process-like Python objects, with a helper to parse
    gen_server-style calls


Building
--------

Requires `gevent` and `greenlet` (dependency of `gevent`) libraries which 
run on both Python 2 and 3. Running `make` will run a simple test which starts
a node at `py@127.0.0.1`.

Source for the documentation is in the `docs/` directory. It uses `sphinx`
documentation generator. In `docs/` run `make html` or just `make`.

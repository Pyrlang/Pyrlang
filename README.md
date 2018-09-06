Pyrlang - Erlang node in Python
===============================

This is a drop-in Erlang node implementation in Python 3.5, designed to allow
interoperation between existing Python projects and BEAM languages: Erlang, 
Elixir, Alpaca, Luaerl, LFE, Clojerl and such. 

With just a few lines of startup code your Python program becomes an Erlang 
network node, participating in the Erlang cluster.

 
Documentation
-------------

http://pyrlang.readthedocs.io/en/latest/


Features
--------

*   Requires Python 3.5 due to type hints, in theory could work on Python 2 and 
    Gevent, but currently no work is done to ensure Python 2 compatibility
*   Supports switchable backends: Gevent and Asyncio - see examples.
*   Erlang distribution protocol for Erlang versions 19, 20, and 21
*   Registry of Python 'processes', which are asyncronous tasks, have a pid
    and an optional registered name
*   User can inherit from `Pyrlang.Process` to create simple process-like Python 
    objects which can receive messages, be linked and monitored from Erlang
*   Send and receive messages locally and remotely by pid or name
*   Supports `net_adm` pings
*   Supports RPC calls. An RPC call can propagate an exception from 
    Python to Erlang;
*   `Pyrlang.GenServer` descendant from `Pyrlang.Process` allows accepting
    generic calls mapped to Python class members
*   For low level operation `Pyrlang.gen` module helps decode `gen`-style calls 
    

Building
--------

Install requirements via: `pip3 install -r requirements.txt`, this will also
pull requirements for Sphinx documentation generator (not needed if you only
use the library).

To operate Pyrlang only requires `gevent` and `greenlet` (dependency of 
`gevent`) OR `asyncio`.
NOTE: type specs in Pyrlang code require Python 3.5 so it will not
really work on Python 2. 

Source for the documentation is in the `docs/` directory. It uses `sphinx`
documentation generator. To generate: run `make docs` or in `docs/` directory
you can run `make` to see other output formats.


Examples
--------

See the documentation page for Examples description.
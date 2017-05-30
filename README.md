Pyrlang
=======

This is a drop-in Erlang node implementation in Python, designed to allow
access to existing Python projects from Erlang and the opposite. 
With just a few lines of startup code your Python program becomes a fully
functional Erlang node.
 
Features
--------

*   Erlang node (hidden or normal mode), only incoming connections for now.
*   Send and receive messages between Erlang processes and Python objects.
*   Can remotely call Python callables (like object constructors or 
    functions).

Documentation
-------------

http://pyrlang.readthedocs.io/en/latest/

Building
--------

Requires `gevent` and `greenlet` (dependency of `gevent`) libraries which 
run on both Python 2 and 3.

Source for the documentation is in the `docs/` directory. It uses `sphinx`
documentation generator.

Pyrlang
=======

This is a drop-in Erlang node implementation in Python, designed to allow
access to existing Python projects from Erlang and the opposite. 
With just a few lines of startup code your Python program becomes a fully
functional Erlang node.
 
Features
--------

*   Erlang node (hidden or normal mode)
*   Send and receive messages between Erlang processes and Python objects
*   Spawn Python objects with arguments remotely from Erlang

Building
--------

Requires `gevent` and `greenlet` (dependency of `gevent`) libraries which 
run on both Python 2 and 3.

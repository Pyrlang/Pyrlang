Python RPC Library for Pyrlang
==============================

The ``py`` module has no external dependencies and can be copied into your
project, renamed if you want, and used directly at any time.
It supports remote notebook-style calls from Erlang to Python, and batching
calls.

Requires Pyrlang to be running on the remote side.

NOTE: Ordinary Erlang-style `rpc:call` is supported by Pyrlang out of the 
box and does not require this library.

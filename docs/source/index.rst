.. Pyrlang Documentation master file, created by
   sphinx-quickstart on Mon May 29 12:12:47 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Pyrlang Library
===============

Pyrlang is a Python library which implements Erlang distribution protocol and
creates an Erlang-compatible node in your Erlang cluster.

You can send and receive messages, spawn "processes" on Python side, which
will be addressable from Erlang using familiar Erlang concepts like message
sending to process identifiers or registered names.
Also same works from Python: you can address Erlang processes, send messages
to them, monitor and link with them.

The library is designed to be dropped into existing code base with very few
changes required.

.. toctree::
    :maxdepth: 2
    :caption: Contents:

    examples
    configuration
    cookbook
    data_types
    calling_python


Pyrlang Modules
---------------

.. toctree::
    :maxdepth: 1

    node
    process
    rex
    gen
    net_kernel
    gen_server
    Notebook.notebook
    Engine.base_engine
    Engine.base_protocol
    Engine.asyncio_engine
    Engine.gevent_engine


Erlang Data Types in Pyrlang
----------------------------

.. toctree::
    :maxdepth: 1

    Term.atom
    Term.bitstring
    Term.erl_typing
    Term.fun
    Term.list
    Term.pid
    Term.reference


Distribution Protocol Implementation
------------------------------------

.. toctree::
    :maxdepth: 1

    Dist.distribution
    Dist.epmd
    Dist.etf
    Dist.node_opts
    Dist.in_dist_protocol
    Dist.out_dist_protocol
    Dist.base_dist_protocol


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

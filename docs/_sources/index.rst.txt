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

    build-library
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
    pyrlang.notebook
    pyrlang.util
    async.base_engine
    async.base_protocol
    async.asyncio_engine
    async.gevent_engine


Distribution Protocol Implementation
------------------------------------

.. toctree::
    :maxdepth: 1

    dist.distribution
    dist.epmd
    dist.distflags
    dist.in_dist_protocol
    dist.out_dist_protocol
    dist.base_dist_protocol


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

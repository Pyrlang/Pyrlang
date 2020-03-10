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
    :maxdepth: 3

    modules/index


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

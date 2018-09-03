Examples!
=========

Talk is cheap! Show me some code!

Have a look in :doc:`getting_started` too!

Some examples are located in ``examples/`` directory. And there are convenient
``Makefile`` targets to call them, just use your Bash autocomplete when typing
``make example<TAB>``:

*   ``make example1`` - will start a node at ``py@127.0.0.1`` and using cookie
    ``COOKIE`` will try to connect to Erlang running at ``erl@127.0.0.1``;
*   ``make example2`` - will register a process as ``my_process`` and you can
    send messages to it by name from Erlang;
*   ``make example10a`` - will start Python part of Example 10;
*   ``make example10b`` - will start Elixir part of Example 10

.. note::
    Some examples require a client and a server, these have two parts: example A
    and example B.

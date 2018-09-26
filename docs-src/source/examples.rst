Examples!
=========

Talk is cheap! Show me some code!

Have a look in :doc:`cookbook` too!

Some examples are located in ``examples/`` directory. And there are convenient
``Makefile`` targets to call them, just use your Bash autocomplete when typing
``make example<TAB>``:

1: Python to Erlang
-------------------

Demonstrates how Python node can actively find a running Erlang node and connect
to it by attempting to send a message.

Running (in two terminal tabs):

*   Start Erlang node with ``erl -name erl@127.0.0.1 -setcookie COOKIE``.
*   (optional) Register your shell process as ``shell`` by executing in the
    Erlang shell: ``erlang:register(shell, self()).``
*   ``make example1`` - will start a node at ``py@127.0.0.1`` and using cookie
    ``COOKIE`` will try to connect to Erlang running at ``erl@127.0.0.1``;
*   (optional) Check whether Erlang shell received ``hello`` by typing ``flush().``

2: Erlang to Python
-------------------

Demonstrates how to run a named process on Python node. Erlang node will try and
send message to it by name.

*   ``make example2`` - will register a process as ``my_process`` and you can
    send messages to it by name from Erlang;
*   Start Erlang node with ``erl -name erl@127.0.0.1 -setcookie COOKIE``.
*   In Erlang shell try send something to ``my_process`` like this:
    ``{my_process, 'py@127.0.0.1'} ! {hello, 123, [<<"test">>, self()]}.``
*   In Python terminal tab observe log message about incoming data.

3 & 4: Calling/Batch Python
---------------------------

Examples are ``.erl`` files which demonstrate how remote notebook-style calls
and batch calls will work with Erlang and Python.

*   ``make pynode`` (same as ``make example2``) to get a running Python node.
*   ``make example3`` or ``make example4`` and observe what it prints.


5: Links and Monitors of Pyrlang Processes
------------------------------------------

Demonstrates Erlang remotely monitoring and linking to Pyrlang processes.
A Python :py:class:`~pyrlang.process.Process` will trigger monitor and link
messages when exiting.

*   ``make example5a`` will start Erlang node in one terminal and wait
*   ``make example5b`` will start Python part in another terminal, spawn some
    test processes and send a message to Erlang side.
*   Erlang node will link and monitor test processes, try make them exit and
    print the results.


6: Links and Monitors of Erlang Processes
-----------------------------------------

Demonstrates Pyrlang remotely monitoring and linking to Erlang process or node.


10: Elixir and GenServer
------------------------

This example will demonstrate how to accept ``gen_server:call`` in your Python
``Process``.

Running (in two terminal tabs):

*   ``make example10a`` - will start Python part of Example 10 and start a
    :py:class:`~pyrlang.gen_server.GenServer` example;
*   ``make example10b`` - will start Elixir part of Example 10 and perform 2
    ``gen_server:call``'s
*   In Elixir terminal tab observe results for 1st and 2nd calls.

.. note::
    Some examples require a client and a server, these have two parts: example A
    and example B.

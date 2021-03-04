Cookbook - How to Get Started
=============================

You might have come here to see some examples. Very well...
But have a quick look at :doc:`examples` page too!

Start the Node
--------------

.. code-block:: python

    from pyrlang import Node
    from term import Atom

    def main():
        node = Node(node_name="py@127.0.0.1", cookie="COOKIE")

        fake_pid = node.register_new_process()

        # To be able to send to Erlang shell by name first give it a
        # registered name: `erlang:register(shell, self()).`
        # To see an incoming message in shell: `flush().`
        node.send_nowait(sender=fake_pid,
                         receiver=(Atom('erl@127.0.0.1'), Atom('shell')),
                         message=Atom('hello'))

        eng.run_forever()

    if __name__ == "__main__":
        main()


Connect nodes
-------------

.. note:: You can initiate the connection from either Erlang or Python side
    automatically by sending to a remote name using tuple format
    ``{Name, Node}`` or sending to a remote pid (if you have it).

You can initiate the connection between nodes from Erlang side in a different
way. To do this on Erlang side you can use ``net_adm:ping``.

.. code-block:: erlang

    net_adm:ping('py@127.0.0.1').

Also you could send a message to ``{Name, Node}``, where ``Node`` is an
atom like ``'py@127.0.0.1'``, and ``Name`` is a pid or some registered name,
which exists on the Python side.

.. code-block:: erlang

    {Name, 'py@127.0.0.1'} ! hello.

If the process exists on Python side, its ``inbox_`` field (which will be a
Gevent or Asyncio Queue) will receive your message.


Exiting a Pyrlang "Process"
---------------------------

A Pyrlang "process" can exit if:

*   you call Process.exit
*   you call Node.exit_process
*   you link to some other process and it exits
*   remote node calls ``erlang:exit`` with its pid

Because registering a process in the process dictionary introduces
an extra reference to your object, be sure to tell it explicitly
to unregister: call :py:class:`~pyrlang.process.Process`'s method
:py:meth:`~pyrlang.process.Process.exit`.

A more general way to handle both local (by pid or name) and remote processes
(by pid) would be to use :py:class:`~pyrlang.node.Node`'s method
:py:meth:`~pyrlang.node.Node.exit_process`. It can send exit messages to
remotes too.


RPC call from Erlang
--------------------

Python node has the special named process running, called ``'rex'``, which is
necessary for Erlang RPC to work. You can send an RPC call to Python from
Erlang. In the following example ``Pyrlang.logger`` module has a ``tty``
function which will transparently pass all args to the ``print`` operator.

.. code-block:: erlang

    rpc:call('py@127.0.0.1', time, time, []).

.. note::
    You do not need to import module to perform the call, this will be done by Rex.

.. note::
    Module and function name can be atoms, strings (non-unicode) or binaries.

Function call result or error will be sent back to the caller.
In case of error, Erlang exit exception will be created with the exception
value from Python.

``Rex`` also supports gen_server style calls from Erlang:

.. code-block:: erlang

    gen_server:call({rex, 'py@127.0.0.1'},
                    {call, time, time, [], self()}).


Send from Python locally
------------------------

You can send messages using the method
``Node.send(_sender, receiver, message)``, which can deliver messages
locally or remotely.

.. code-block:: python

    node.send(sender=None,  # argument unused
              receiver=term.Atom('my_erlang_process'),
              message=(123, 4.5678, [term.Atom('test')]))

Send from Python to a remote
----------------------------

You can send messages to a remote pid. Sender pid is unused and can be None.
The node connection will be established automatically.

.. code-block:: python

    node.send(sender=None,
              receiver=receiver_pid,
              message=Atom('hello'))

You can send messages to a remote named process, for this use tuple send format
like ``{Name, Node}``. For remote sends sender pid is REQUIRED,
even if it is a fake pid (see example below how to create a fake pid).

To try this, open an Erlang shell and register shell with the name ``'shell'``:

.. code-block:: erlang

    (erl@127.0.0.1) 1> erlang:register(shell, self())

Now we can try and send the message from Python (node connection will be
established automatically):

.. code-block:: python

    fake_pid = node.register_new_process(None)  # create a fake pid
    node.send(sender=fake_pid,
              receiver=(Atom('erl@127.0.0.1'), Atom('shell')),
              message=Atom('hello'))

.. code-block:: erlang

    (erl@127.0.0.1) 2> flush().
    Shell got hello
    ok
    (erl@127.0.0.1) 3>

Send to a Python object
-----------------------

A python object inherited from :py:class:`~pyrlang.process.Process` will be
an async task, which adds to the event loop and can coexists with other
tasks. A process is able to register itself (optional) with a name and handle
incoming messages.

Messages sent to a pid or name will be automatically routed to such a
process and arrive into its ``self.inbox_``. The Process base class will
constantly call ``self.handle_inbox()`` so you can check the messages yourself.

.. code-block:: python

    class MyProcess(Process):
        def __init__(self) -> None:
            super().__init__()
            node = self.node_db.get()
            node.register_name(self, Atom('my_process'))  # optional

        def handle_one_inbox_message(self, msg):
            print("Incoming", msg)

.. code-block:: erlang

    %% Now sending from Erlang is easy:
    %% Note that this is syntax for sending to atom names, not to pids!
    (erl@127.0.0.1) 1> {my_process, 'py@127.0.0.1'} ! hello.

    %% If you know the Python pid in Erlang (if you communicated it
    %% from your Python node), then send directly to it:
    (erl@127.0.0.1) 1> PyProcessPid ! hello.


Remote Calculations on Python Node
----------------------------------

**Problem:**
While it is possible to subclass the :py:class:`~pyrlang.process.Process`
class and implement a Erlang-like process, often existing Python code
exposes just a functional API or a class which has to be created for the
calculation to be performed.
Often you would like to use some functional API without sending the results
over the wire until they are ready.

**Solution:**
A notebook-like remote execution API, where intermediate call results are stored
in history log and can be referred by name or index.

There is helper Erlang module called ``py.erl``, please use it and see
:doc:`calling_python` for an example.

.. seealso::
    Example3 in :doc:`examples` demonstrates this.


Batch Remote Calculations on Python Node
----------------------------------------

**Problem:**
Often you would like to use some functional API without sending the results
over the wire until they are ready. Moreover sometimes you might want to run
same batch on multiple nodes, this is possible now too.

Batch remote calculations API allows you to prebuild your calculation as a data
structure on Erlang side and then execute it remotely on one or more
Pyrlang nodes, sending you the final result.
Intermediate call results are stored in history log and can be referred by name
or index.

It is possible to apply the same batch of calls to multiple nodes.

.. seealso::
    Example4 in :doc:`examples` demonstrates this.


Gen_server-like Processes
-------------------------

To have a :py:class:`~pyrlang.process.Process` descendant which responds to
``gen_server:call``, inherit your class from :py:class:`~pyrlang.gen.server.GenServer`.
When calling ``GenServer`` constructor in your ``__init__`` specify an
additional parameter ``accepted_calls`` which is a list of strings.

Functions with these names will be mapped to incoming ``gen_server:call``
and their result will be transparently 'replied' back to the caller.

.. code-block:: python

    from pyrlang.gen.decorators import call
    class MyGenServer(GenServer):
        def __init__(self):
            super().__init__()
            node = self.node_db.get()
            node.register_name(self, Atom('my_gen_server'))  # optional

        @call(1, lambda msg: msg == 'hello')
        def hello(self, msg):
            return self.pid_

        @call(2, lambda msg: True)
        def catch_all_call(self, msg):
            ret = "I don't understand"
            return ret

The decorator will be used by the meta class so that when a call comes it matches
all functions that have ``call`` decorator. The first number is the priority, just
as in erlang, where the first function clause that matches is the one that gets the
call.

.. code-block:: erlang

    Server = {my_gen_server, 'py@127.0.0.1'}.
    gen_server:call(Server, hello).
    gen_server:call(Server, somethingelse).


Linking/Monitoring from Erlang to Python
----------------------------------------

See example6 in :doc:`examples`
for demo on linking a Pyrlang "process" to Erlang process, then
killing it and observing an EXIT signal coming back.

See example7 in :doc:`examples`
for demo on a Pyrlang "process" monitoring a Erlang process, then
killing it and observing the monitor message.


Linking/Monitoring from Python to Erlang
----------------------------------------

See example5 in :doc:`examples`
for demo on how Python can link and monitor local and remote pids.

.. note::
    To link two processes in Pyrlang use :py:class:`~pyrlang.node.Node`'s
    :py:meth:`~pyrlang.node.Node.link` method.

.. note::
    To monitor a process in Pyrlang use :py:class:`~pyrlang.node.Node`'s
    :py:meth:`~pyrlang.node.Node.monitor_process` method.

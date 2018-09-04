Cookbook - How to Get Started
=============================

You might have come here to see some examples. Very well...
But have a quick look at :doc:`examples` page too!

Start the Node
--------------

.. code-block:: python

    from Pyrlang import Node, Atom, GeventEngine # or AsyncioEngine

    def main():
        event_engine = GeventEngine()  # or AsyncioEngine
        node = Node(node_name="py@127.0.0.1", cookie="COOKIE", engine=event_engine)

        fake_pid = node.register_new_process()

        # To be able to send to Erlang shell by name first give it a registered
        # name: `erlang:register(shell, self()).`
        # To see an incoming message in shell: `flush().`
        node.send(sender=fake_pid,
                  receiver=(Atom('erl@127.0.0.1'), Atom('shell')),
                  message=Atom('hello'))

        while True:
            # Sleep gives other greenlets time to run
            event_engine.sleep(0.1)

    if __name__ == "__main__":
        main()

Here ``event_engine`` is a pluggable adapter which allows Pyrlang to run both
with gevent and asyncio-driven event loops. Pyrlang in this case performs mostly
protocols handling, while event engine will open connections, start tasks
and sleep asynchronously.


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


RPC call from Erlang
--------------------

Python node has the special named process running, called ``'rex'``, which is
necessary for Erlang RPC to work. You can send an RPC call to Python from
Erlang. In the following example ``Pyrlang.logger`` module has a ``tty``
function which will transparently pass all args to the ``print`` operator.

.. code-block:: erlang

    rpc:call('py@127.0.0.1', 'Pyrlang.logger', 'tty', ["Hello World"]).

Function call result or error will be sent back to the caller.
In case of error, Erlang exit exception will be created with the exception
value from Python.

``Rex`` also supports gen_server style calls from Erlang:

.. code-block:: erlang

    gen_server:call({rex, 'py@127.0.0.1'},
                    {call, 'Pyrlang.logger', tty, ["Hello"], self()}).


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

A python object inherited from :py:class:`~Pyrlang.process.Process` will be
a Greenlet (i.e. running in parallel with the rest of the system).
A process is able to register itself (optional) with a name and handle
incoming messages.

Messages sent to a pid or name will be automatically routed to such a
process and arrive into its ``self.inbox_``. The Process base class will
constantly call ``self.handle_inbox()`` so you can check the messages yourself.

.. note:: Because registering a process in the process dictionary introduces
    an extra reference to your object, be sure to tell it explicitly
    to unregister: call ``self.exit(reason=None)`` (defined in Process class).

.. code-block:: python

    from Pyrlang import Node, Atom, Process, GeventEngine # or AsyncioEngine

    class MyProcess(Process):
        def __init__(self, node) -> None:
            Process.__init__(self, node)
            node.register_name(self, Atom('my_process'))  # optional
            print("registering process - 'my_process'")

        def handle_one_inbox_message(self, msg):
            print("Incoming", msg)

    def main():
        event_engine = GeventEngine()  # or AsyncioEngine
        node = Node(node_name="py@127.0.0.1", cookie="COOKIE", engine=event_engine)

        # this automatically schedules itself to run via gevent
        mp = MyProcess(node)
        while True:
            event_engine.sleep(0.1)

    if __name__ == "__main__":
        main()

Now sending from Erlang is easy:

.. code-block:: erlang

    (erl@127.0.0.1) 1> {my_process, 'py@127.0.0.1'} ! hello.


TODO Remote Calculations on Python Node
---------------------------------------

**Problem:**
While it is possible to subclass the :py:class:`~Pyrlang.process.Process`
class and implement a Erlang-like process, often existing Python code
exposes just a functional API or a class which has to be created for the
calculation to be performed.
Often you would like to use some functional API without sending the results
over the wire until they are ready.

**Solution:**
A notebook-like remote execution API, where intermediate call results are stored
in history log and can be referred by name or index.

.. todo::
    Describe how chain of calculations can be performed remotely in
    **direct mode** (one by one) using the new API.


TODO Lazy Remote Calculations on Python Node
--------------------------------------------

**Problem:**
Same as with direct remote calculations: Often you would like to use some
functional API without sending the results over the wire until they are ready.
Lazy remote calculations API allows you to prebuild your calculation as a data
structure on Erlang side and then execute it remotely on one or more
Pyrlang nodes, sending you the final result.
Intermediate call results are stored in history log and can be referred by name
or index.

.. todo::
    Describe how to calculate chain of calls on a remote node **lazily**
    using the new API.


TODO Implement a Gen_server-like Object
---------------------------------------

.. todo::
    This section needs to be updated when GenServer is added

Getting Started
===============

You might have come here to see some examples. Very well...

Start the Node
--------------

.. code-block:: python

    import gevent
    from gevent import monkey
    monkey.patch_all()

    import Pyrlang

    def main():
        node = Pyrlang.Node("py@127.0.0.1", "COOKIE")
        node.start()

        while True:
            # Sleep gives other greenlets time to run
            gevent.sleep(0.1)

    if __name__ == "__main__":
        main()


Connect from Erlang
-------------------

You can initiate the connection between nodes from Erlang side. To do this,
on Erlang side you can use ``net_adm:ping``.

.. code-block:: erlang

    net_adm:ping('py@127.0.0.1').

Also you could send a message to ``{Node, Name}``, where ``Node`` is an
atom like ``'py@127.0.0.1'``, and ``Name`` is a pid or some registered name,
which exists on the Python side.

.. code-block:: erlang

    {'py@127.0.0.1', Name} ! hello.

If the process exists on Python side, its ``inbox_`` field (which must be a
Gevent Queue) will receive your message. You can check it from your code
using ``self.inbox_.empty()`` and the family of ``.get*()`` functions
which can wait or won't wait for another message.


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

Send from Python
----------------

You can send messages in the reverse direction too!
``Node.send(_sender, receiver, message)`` function is there to deliver messages
locally or remotely.

.. code-block:: python

    node.send(sender=None,  # argument unused
              receiver=term.Atom('my_erlang_process'),
              message=(123, 4.5678, [term.Atom('test')]))

Note: Tuple format ``{Node, Name}`` for sending is not supported.

Note: Node is a singleton, you can find the node by referencing
    ``Node.singleton``. This may change in future.

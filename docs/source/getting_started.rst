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


Send from Python (local)
------------------------

You can send messages in the reverse direction too!
``Node.send(_sender, receiver, message)`` function is there to deliver messages
locally or remotely.

.. code-block:: python

    node.send(sender=None,  # argument unused
              receiver=term.Atom('my_erlang_process'),
              message=(123, 4.5678, [term.Atom('test')]))

.. note:: Node is a singleton, you can find the node by referencing
    ``Node.singleton``. This may change in future.

Send from Python (remote)
-------------------------

You can send messages to a remote pid. Sender pid is unused and can be None.
The node connection will be established automatically.

.. code-block:: python

    node.send(sender=None,
              receiver=receiver_pid,
              message=Atom('hello'))

You can send messages to a remote named process, for this use tuple send format
like ``{Node, Name}``. For this sender pid is REQUIRED and must be provided,
even if it is a fake (see example below how to create a fake pid).

.. code-block:: python

    pid = node.register_new_process(None)
    node.send(sender=pid,
              receiver=(Atom('erl@127.0.0.1'), Atom('shell')),
              message=Atom('hello'))


Implement a Gen_server-like Object
----------------------------------

It is not very hard to implement minimum interface required to be able to
respond to ``gen:call``, which is used by ``gen_server`` in Erlang/OTP.

Process class has a ``_run`` function which calls ``self.handle_inbox()``
repeatedly.
:py:class:`~Pyrlang.mailbox.Mailbox`
class offers ``receive_wait(filter_fn)``
for selective receive with a wait, ``receive(filter_fn)`` for instant mailbox
check selectively and simple ``get()`` and ``get_nowait()`` functions.

.. code-block:: python

    from Pyrlang.process import Process

    class MyProcess(Process):
        def __init__(self, node) -> None:
            Process.__init__(self, node)
            node.register_name(self, term.Atom('my_process'))  # optional

        def handle_inbox(self):
            while True:
                # Do a selective receive but the filter says always True
                msg = self.inbox_.receive(filter_fn=lambda _: True)
                if msg is None:
                    break
                self.handle_one_inbox_message(msg)

        def handle_one_inbox_message(self, msg) -> None:
            gencall = gen.parse_gen_message(msg)
            if isinstance(gencall, str):
                print("MyProcess:", gencall)
                return

            # Handle the message in 'gencall' using its sender_, ref_ and
            # message_ fields

            if EVERYTHING_IS_OK:
                # Send a reply
                gencall.reply(local_pid=self.pid_,
                              result=SOME_RESULT_HERE)

            else:
                # Send an error exception which will crash Erlang caller
                gencall.reply_exit(local_pid=self.pid_,
                                   reason=SOME_ERROR_HERE)

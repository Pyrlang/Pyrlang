#
# Start a simple node and connect to an Erlang/Elixir node.
# This Pyrlang node will be visible as `py@127.0.0.1`.
#
# 1. Run `make erlshell` or `erl -name erl@127.0.0.1 -setcookie COOKIE`
# 2. In Erlang shell: `erlang:register(shell, self()).`
# 3. In another terminal window: `make example1`
# 4. In Erlang shell: `net_adm:ping('py@127.0.0.1').`
#
# Shell process will receive 'hello' (type `flush().` to see)
#

import logging
from pyrlang2 import Node
from term import Atom
from colors import color

LOG = logging.getLogger(color("EXAMPLE1", fg='lime'))


def main():
    event_engine = Engine()
    node = Node(node_name="py@127.0.0.1", cookie="COOKIE", engine=event_engine)

    fake_pid = node.register_new_process()

    # To be able to send to Erlang shell by name first give it a registered
    # name: `erlang:register(shell, self()).`
    # To see an incoming message in shell: `flush().`
    node.send(sender=fake_pid,
              receiver=(Atom('erl@127.0.0.1'), Atom('shell')),
              message=Atom('hello'))

    event_engine.run_forever()


if __name__ == "__main__":
    main()

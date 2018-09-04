#
# Start a simple node and connect to an Erlang/Elixir node.
# This Python node is visible as `py@127.0.0.1`.
#
# Requires:     Erlang running on the same host as:
#               `erl -name erl@127.0.0.1 -setcookie COOKIE`
# Run:          from project root run `make example1`
# Try in Erlang shell: `net_adm:ping('py@127.0.0.1').`
#
# Before starting example1 try in Erlang shell: `erlang:register(shell, self()).`
# Shell process will receive 'hello' (type `flush().` to see)
#
import sys
sys.path.insert(0, ".")

from Pyrlang import Node, Atom, GeventEngine


def main():
    event_engine = GeventEngine()
    node = Node(node_name="py@127.0.0.1", cookie="COOKIE", engine=event_engine)
    event_engine.start_task(node)

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

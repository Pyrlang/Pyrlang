#
# Start a simple node and connect to an Erlang/Elixir node.
# This Python node is visible as `py@127.0.0.1`.
#
# Requires:     Erlang running on the same host as:
#               `erl -name erl@127.0.0.1 -setcookie COOKIE`
# Run:          from project root run `make example1`
# Try in Erlang shell: `net_adm:ping('py@127.0.0.1').`
#
import sys
sys.path.insert(0, ".")

from Pyrlang import Node, Atom, GeventEngine


def main():
    event_engine = GeventEngine()
    node = Node(node_name="py@127.0.0.1", cookie="COOKIE", engine=event_engine)
    event_engine.start_task(node)

    # Attempt to send something will initiate a connection before sending
    pid = node.register_new_process(None)

    # To be able to send to Erlang shell by name first give it a registered
    # name: `erlang:register(shell, self()).`
    node.send(pid, (Atom('erl@127.0.0.1'), Atom('shell')), Atom('hello'))

    while True:
        # Sleep gives other greenlets time to run
        event_engine.sleep(0.1)


if __name__ == "__main__":
    main()

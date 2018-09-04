#
# This example shows:
# 1. Creating a Python object based on `Process` class, and registering it with
#   a name.
# 2. A remote node can send to this process by name or Pid.
# 3. On incoming message MyProcess'es `handle_inbox` will find that there is a
#   message and will call `handle_one_inbox_message` which is overridden here.
# 4. There is no way to know sender unless you add return address into the msg.
#
# Requires:     Erlang running on the same host as:
#               `erl -name erl@127.0.0.1 -setcookie COOKIE`
# Run:          from project root run `make example2`
# Erl command:  {my_process, 'py@127.0.0.1'} ! hello.
#

import sys
sys.path.insert(0, ".")

from Pyrlang import Node, Atom, Process
from Pyrlang import GeventEngine as Engine
# from Pyrlang import AsyncioEngine as Engine


class MyProcess(Process):
    def __init__(self, node) -> None:
        Process.__init__(self, node)
        node.register_name(self, Atom('my_process'))  # optional
        print("Example2: registering process - 'my_process'")

    def handle_one_inbox_message(self, msg):
        print("Example2: Incoming", msg)


def main():
    event_engine = Engine()
    node = Node(node_name="py@127.0.0.1", cookie="COOKIE", engine=event_engine)
    MyProcess(node)
    event_engine.run_forever()


if __name__ == "__main__":
    main()

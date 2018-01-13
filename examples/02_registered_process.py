#
# This example shows:
# 1. Creating a Python object based on `Process` class, and registering it with
#   a name.
# 2. A remote node can send to this process by name or Pid.
# 3. On incoming message MyProcess'es `handle_inbox` will find that there is a
#   message and will call `handle_one_inbox_message` which is overridden here.
# 4. There is no way to know sender unless you add return address into the msg.
#
# Erlang command -> {my_process, 'py@127.0.0.1'} ! hello.
#

import sys
sys.path.insert(0, ".")

import gevent
from gevent import monkey
monkey.patch_all()
import Pyrlang
from Pyrlang import Atom
from Pyrlang import Process


class MyProcess(Process):
    def __init__(self, node) -> None:
        Process.__init__(self, node)
        node.register_name(self, Atom('my_process'))  # optional
        print("registering process - 'my_process'")

    def handle_one_inbox_message(self, msg):
        print("Incoming", msg)


def main():
    node = Pyrlang.Node("py@127.0.0.1", "COOKIE")
    node.start()
    mp = MyProcess(node)
    while True:
        gevent.sleep(0.1)


if __name__ == "__main__":
    main()

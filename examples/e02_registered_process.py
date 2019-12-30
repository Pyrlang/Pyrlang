#
# This example shows:
# 1. Creating a Python object based on `Process` class, and registering it with
#   a name.
# 2. A remote node can send to this process by name or Pid.
# 3. On incoming message MyProcess'es `handle_inbox` will find that there is a
#   message and will call `handle_one_inbox_message` which is overridden here.
# 4. There is no way to know sender unless you add return address into the msg.
#
# Steps:
# 1. Run `make erlshell` or `erl -name erl@127.0.0.1 -setcookie COOKIE`
# 2. Run `make example2` in another terminal
# 3. In Erlang shell send a message `{my_process, 'py@127.0.0.1'} ! hello`
#

import logging

from term import Atom
from pyrlang.node import Node
from pyrlang.process import Process
from colors import color

LOG = logging.getLogger(color("EXAMPLE2", fg='lime'))
logging.getLogger("").setLevel(logging.DEBUG)


class MyProcess(Process):
    def __init__(self) -> None:
        Process.__init__(self)
        self.get_node().register_name(self, Atom('my_process'))  # optional
        LOG.info("Registering process - 'my_process'")

    def handle_one_inbox_message(self, msg):
        LOG.info("Incoming %s", msg)


def main():
    n = Node(node_name="py@127.0.0.1", cookie="COOKIE")
    MyProcess()
    n.run()


if __name__ == "__main__":
    main()

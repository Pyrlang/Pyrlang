#
# A simple Python server and an Elixir client sending to it
# Python server will reply with its own Pid, so then you know the Pid and can
# send to it directly (second send call).
#
# Run `make example10a` to run Python node
# Run `make example10b` to run Elixir client which will perform the call
#

import logging

from term import Atom
from pyrlang.gen.server import GenServer
from pyrlang.gen.decorators import call, cast, info
from pyrlang import Node

LOG = logging.getLogger("+++EXAMPLE10+++")
logging.getLogger("").setLevel(logging.DEBUG)


class MyProcess(GenServer):
    def __init__(self, node) -> None:
        super().__init__()
        node.register_name(self, Atom('my_process'))
        LOG.info("registering process - 'my_process'")

    @call(1, lambda msg: msg == b'hello')
    def hello(self, msg):
        """ This is called via ``gen_server:call`` """
        return self.pid_

    @call(2, lambda msg: msg == b'hello_again')
    def hello_again(self, msg):
        """ This is called from Elixir test after ``hello`` returned success. """
        return b'Approved!'

    @call(3, lambda msg: True)
    def hello_catch_all(self, msg):
        """Catch all handler"""
        return b'I\'m unsure how to respond!'

def main():
    node = Node(node_name="py@127.0.0.1", cookie="COOKIE")

    MyProcess(node)

    node.run()


if __name__ == "__main__":
    main()

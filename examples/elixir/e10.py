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
from pyrlang.gen_server import GenServer
from pyrlang import Node
from pyrlang import GeventEngine as Engine
# from pyrlang import AsyncioEngine as async

LOG = logging.getLogger("+++EXAMPLE10+++")
logging.getLogger("").setLevel(logging.DEBUG)


class MyProcess(GenServer):
    def __init__(self, node) -> None:
        GenServer.__init__(self, node.node_name_,
                           accepted_calls=['hello', 'hello_again'])
        node.register_name(self, Atom('my_process'))
        LOG.info("registering process - 'my_process'")

    def hello(self):
        """ This is called via ``gen_server:call`` """
        return self.pid_

    @staticmethod
    def hello_again():
        """ This is called from Elixir test after ``hello`` returned success. """
        return b'Approved!'


def main():
    event_engine = Engine()
    node = Node(node_name="py@127.0.0.1", cookie="COOKIE", engine=event_engine)

    MyProcess(node)

    event_engine.run_forever()


if __name__ == "__main__":
    main()

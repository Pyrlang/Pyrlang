#
# A simple Python server and an Elixir client sending to it
# Python server will reply with its own Pid, so then you know the Pid and can
# send to it directly (second send call).
#
# Run `make example10a` to run Python node
# Run `make example10b` to run Elixir client which will perform the call
#

import sys
sys.path.insert(0, ".")

import logging
from Pyrlang import Node, Atom, Process, gen
from Pyrlang import GeventEngine as Engine
# from Pyrlang import AsyncioEngine as Engine

LOG = logging.getLogger("+++EXAMPLE10+++")
LOG.setLevel(logging.DEBUG)


class MyProcess(Process):
    def __init__(self, node) -> None:
        Process.__init__(self, node)
        node.register_name(self, Atom('my_process'))
        LOG.info("registering process - 'my_process'")

    def handle_one_inbox_message(self, msg):
        LOG.info("Incoming to %s %s %s", self.pid_, type(self.pid_), msg)
        gencall = gen.parse_gen_message(msg, node_name=self.node_name_)

        if isinstance(gencall, str):
            LOG.error("MyProcess gen parse error: %s", gencall)
            return
        else:
            LOG.info("Incoming gen_call %s", gencall)
            # Here handle_call would match on the `message_` field of gencall

        LOG.info("replying with my pid %s", self.pid_)
        gencall.reply(local_pid=self.pid_, result=self.pid_)


def main():
    event_engine = Engine()
    node = Node(node_name="py@127.0.0.1", cookie="COOKIE", engine=event_engine)

    MyProcess(node)

    event_engine.run_forever()


if __name__ == "__main__":
    main()


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

import gevent
from gevent import monkey
monkey.patch_all()
import Pyrlang
from Pyrlang import Atom, Process, gen


class MyProcess(Process):
    def __init__(self, node) -> None:
        Process.__init__(self, node)
        node.register_name(self, Atom('my_process'))
        print("registering process - 'my_process'")

    def handle_one_inbox_message(self, msg):
        print("Incoming to", self.pid_, type(self.pid_), msg)
        gencall = gen.parse_gen_message(msg, node_name=self.node_name_)

        if isinstance(gencall, str):
            print("MyProcess:", gencall)
            return

        print('replying pid')
        gencall.reply(local_pid=self.pid_, result=self.pid_)


def main():
    node = Pyrlang.Node("py@127.0.0.1", "COOKIE")
    node.start()

    mp = MyProcess(node)

    while True:
        # Sleep gives other greenlets time to run
        gevent.sleep(0.5)


if __name__ == "__main__":
    main()


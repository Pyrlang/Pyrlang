# Starts a pynode and registers a pyrlang gen_serevr process
# then wait and let the erlang part do it's job

import asyncio
from pyrlang2.node import Node
from pyrlang2.gen import GenServerInterface
from pyrlang2.gen_server import GS, call, cast, info

from term import Atom

import logging
from colors import color

LOG = logging.getLogger(color("EXAMPLE1", fg='lime'))
logging.getLogger("").setLevel(logging.DEBUG)


class Server(GS):
    """
    Simple GenServer class that handles some data
    """
    def __init__(self):
        super().__init__()
        mypid = self.pid_
        n = self.get_node()
        n.register_name(self, Atom('pysrv'))
        n.send_nowait(mypid, mypid, "register")
        self.other_pid = None

    @call(100, lambda msg: True)
    def handle_call(self, msg):
        LOG.info("got unknown call")
        return "don't understand"

    @call(1, lambda msg: msg == Atom('who_are_you'))
    def who_are_you(self, msg):
        LOG.info("got who are you question")
        return self.get_node().node_name_

    @call(2, lambda msg: msg == Atom('who_is_the_other_one'))
    async def names_do_not_matter(self, msg):
        if not self.other_pid:
            return "don't know yet"
        gsi = GenServerInterface(self, self.other_pid)
        res = await gsi.call(Atom('who_are_you'))
        LOG.info("got response for who are you %s", res)
        return "The other one is", res

    @cast(1, lambda msg: type(msg) == tuple and msg[0] == Atom("other_py_node"))
    def call_other_py_node(self, msg):
        other = msg[1]
        LOG.info("got the other py nodes pid %s", other)
        self.other_pid = other

    @cast(100, lambda msg: True)
    def handle_cast(self, msg):
        LOG.info("got unknown cast %s", msg)

    @info(100, lambda msg: True)
    def handle_info(self, msg):
        LOG.info("got unknown info %s", msg)

    @info(0, lambda msg: msg == 'register')
    def register(self, msg):
        LOG.info("registering with erl node")
        n = self.get_node()
        dest = (Atom('erl@127.0.0.1'), Atom('example8'))
        n.send_nowait(self.pid_, dest, (Atom('register'), self.pid_))


def init_server():
    LOG.info("starting server")
    s = Server()


def init(name):
    ee = asyncio.get_event_loop()
    n = Node(node_name=name, cookie="COOKIE")
    ee.call_soon(init_server)
    return ee, n


def main():
    import sys
    name = sys.argv[1]
    e, n = init(name)
    e.run_forever()


if __name__ == '__main__':
    main()


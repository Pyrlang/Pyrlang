import unittest
import asyncio
from os import path
from time import sleep
from multiprocessing import Process as OsProc
from subprocess import Popen, PIPE

# we import the db class because we're going to do nasty stuff
from pyrlang import node_db
from pyrlang.process import Process
from pyrlang.gen.server import GenServer
from pyrlang.gen.server import GenServerInterface as GSI
from pyrlang.gen.decorators import call, cast, info
from pyrlang import Node
from pyrlang.dist_proto import base_dist_protocol
from term import Atom

TESTCOOKIE="PYRLANGTESTCOOKIE"


async def _set_future_after(fut, delay):
    await asyncio.sleep(delay)
    fut.set_result(True)


def run_loop_for(node, delay):
    loop = node.get_loop()
    fut = loop.create_future()
    loop.create_task(_set_future_after(fut, delay))
    node.run(fut)


def start_ext_py_node(setting=None):
    dpath = path.dirname(path.abspath(__file__))
    pth = path.join(dpath, 'ext_node.py')
    #print('executing  {}'.format(pth))
    if setting:
        p = Popen([pth, setting], stdout=PIPE, stderr=PIPE)
    else:
        p = Popen([pth], stdout=PIPE, stderr=PIPE)
    return p


class TestNodeDbFunctionality(unittest.TestCase):
    def test_defining_multiple_nodes(self):
        """
        Super internal functionality

        Although this is in general a bad idea, it's nice for edgy things and
        testing
        """
        db = node_db.db
        n1 = Node("py1@127.0.0.1", cookie="COOKIE")
        with self.assertRaises(AttributeError) as context:
            Node("py2@127.0.0.1", cookie="COOKIE")
        self.assertTrue("there is already an active node: py1@127.0.0.1",
                        str(context.exception))
        db.deactivate(n1)
        n2 = Node("py2@127.0.0.1", cookie="COOKIE")
        n1.destroy()
        n2.destroy()
        del n1
        del n2


class NodeTests(unittest.TestCase):
    ext_setup = None
    p = None
    node = None

    @classmethod
    def setUpClass(cls) -> None:
        #print("\n\n\nnew test class init\n\n")
        cls.p = start_ext_py_node(cls.ext_setup)
        sleep(1)
        #print("\ndone waiting for ext node\n\n")
        cls.node = Node('py@127.0.0.1', TESTCOOKIE)
        # run node for a while to give some time for epmd to connect
        run_loop_for(cls.node, 1)

    @classmethod
    def tearDownClass(cls) -> None:
        cls.p.terminate()
        out, err = cls.p.communicate()
        #print("\n\nkilled ext node")
        #print("stdout:\n", out.decode('utf8'), "\n\n")
        #print("stderr:\n", err.decode('utf8'), "\n\next process info done")

        cls.node.shut_down_nowait()
        cls.node.run()


class RetServer(GenServer):
    r_server = None
    make_it_crash = False

    def __init__(self, name='retproc'):
        super().__init__()
        node_db.get().register_name(self, Atom(name))

    @cast(1, lambda msg: True)
    async def handle_cast(self, msg):
        #print("I'm going to sleep now!!")
        await asyncio.sleep(10)
        #print("Done sleeping")

    @info(1, lambda msg: msg == 'start')
    def start(self, msg):
        pass
        #print(self, "got message: ", msg)

    @info(2, lambda msg: msg == 'link')
    async def prep_link(self, msg):
        #print(self, "prepping for linking")
        node = node_db.get()
        await node.send(self.pid_, (Atom('py_ext@127.0.0.1'),
                                     Atom('namedserver')),
                         (Atom('sendpid'), self))

    @info(3, lambda msg: type(msg) == tuple and msg[0] == 'namedpid')
    async def finalize_link(self, msg):
        pid = msg[1]
        #print("got pid back", pid)
        await node_db.get().link(self.pid_, pid)
        self.r_server = GSI(self, pid)
        if self.make_it_crash:
            await self.r_server.cast("crash")

    @info(4, lambda msg: msg == "crashremoteserver")
    async def crashremoteserver(self, msg):
        self.make_it_crash = True


class StartStopNode(unittest.TestCase):

    def test_start_stop(self):
        n = Node('py@127.0.0.1', TESTCOOKIE)
        s = RetServer()
        run_loop_for(n, 0.2)
        #print("jso \n\n\n", n.processes_, "\n\n\n")
        self.assertTrue(s.pid_ in n.processes_)
        self.assertFalse(s._run_task.done())
        self.assertEqual(node_db.get(), n)
        n.shut_down_nowait()
        n.run()
        self.assertTrue(s._run_task.done())
        self.assertEqual({}, n.processes_)
        self.assertRaises(AttributeError, node_db.get)


class TestProcessCrash(NodeTests):
    ext_setup = 'process_crash'

    def test_proc_exit(self):
        s = RetServer()
        pid = s.pid_
        node = node_db.get()
        node.send_nowait(pid, pid, 'crashremoteserver')
        node.send_nowait(pid, pid, 'link')
        run_loop_for(node, 1)
        self.assertTrue(s._run_task.cancelled())
        self.assertTrue(s._run_task.done())
        self.assertRaises(asyncio.CancelledError, s._run_task.exception)


class TrackInboxProcess(Process):
    def __init__(self):
        super().__init__()
        self.msgs = []

    def handle_one_inbox_message(self, msg):
        self.msgs.append(msg)


class TestNodeLost(NodeTests):
    ext_setup = 'node_lost'

    def test_node_idle_remove(self):
        # force the timeout to happen before we get ping back
        idle = base_dist_protocol.IDLE_TIMEOUT
        alive = base_dist_protocol.ALIVE_CHECK_TIME
        base_dist_protocol.IDLE_TIMEOUT = 1
        base_dist_protocol.ALIVE_CHECK_TIME = 0.5
        p = TrackInboxProcess()
        node = node_db.get()
        dest = (Atom('py_ext@127.0.0.1'), Atom('namedserver'))
        msg = (Atom('sendpid'), p.pid_)
        node.send_nowait(p.pid_, dest, msg)
        run_loop_for(node, 2)
        self.assertEqual(node.dist_nodes_,
                         {},
                         "make sure the ext node is removed")
        self.assertEqual(len(p.msgs), 1)
        node.send_nowait(p.pid_, dest, msg)
        run_loop_for(node, 0.5)
        self.assertTrue('py_ext@127.0.0.1' in node.dist_nodes_,
                        "make sure the ext node is removed")
        self.assertEqual(len(p.msgs), 2)
        run_loop_for(node, 3)
        self.assertEqual(node.dist_nodes_,
                         {},
                         "make sure the ext node is removed")
        base_dist_protocol.IDLE_TIMEOUT = idle
        base_dist_protocol.ALIVE_CHECK_TIME = alive


def set_logger():
    import logging
    import sys
    root = logging.getLogger()
    root.setLevel(logging.DEBUG)

    handler = logging.StreamHandler(sys.stdout)
    handler.setLevel(logging.DEBUG)
    formatter = logging.Formatter(
        '%(asctime)s - %(name)s:%(lineno)d [%(process)d]- %('
        'levelname)s - %(message)s')
    handler.setFormatter(formatter)
    root.addHandler(handler)


if __name__ == '__main__':
    # set logging when you want/need more output
    # set_logger()
    unittest.main(exit=True)

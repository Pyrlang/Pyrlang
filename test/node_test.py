import unittest

from time import sleep

from Pyrlang import Node, Atom
from Pyrlang.Engine.base_engine import BaseEngine
from Pyrlang.Engine.asyncio_engine import AsyncioEngine
# from Pyrlang.Engine.gevent_engine import GeventEngine


def start_stop_with(ev_engine: BaseEngine, py_node_name: str):
    """ Starts the local node node, tries to connect to an externally started
        Erlang node, then stops the local node. Checks whether reference to it
        died (whether GC was able to claim it).
        TODO: It doesn't always claim it. Probably not a bug?
    """
    node = Node(node_name=py_node_name + "@127.0.0.1",
                cookie="COOKIE",
                engine=ev_engine)

    fake_pid = node.register_new_process()
    node.send(sender=fake_pid,
              receiver=(Atom('erl@127.0.0.1'), Atom('shell')),
              message=Atom('hello'))

    ev_engine.sleep(3)

    import weakref
    import gc

    wref = weakref.ref(node)
    node.destroy()
    del node
    gc.collect()
    assert wref() is None, "Reference to node must be dead at this point"


class TestNodeFunctions(unittest.TestCase):
    def __init__(self, methodName):
        super().__init__(methodName=methodName)
        self.engine_ = AsyncioEngine()
        self.node_name_ = "py@127.0.0.1"
        self.node_ = Node(node_name=self.node_name_,
                          cookie="COOKIE",
                          engine=self.engine_)

    # def test_start_stop_as_asyncio(self):
    #     start_stop_with(AsyncioEngine(), "py_asyncio")

    # def test_start_stop_as_gevent(self):
    #     self.start_stop_with(GeventEngine(), "py_gevent")

    def test_notebook_call(self):
        from Pyrlang.Notebook import Notebook
        nb = Notebook(options={}, node_name=self.node_name_)

        # Try calling with different styles of path
        _, value1 = nb.nb_call(
            {Atom("path"): ["datetime", "datetime", "now"],
             Atom("args"): [],
             Atom("kwargs"): {},
             Atom("immediate"): True})

        _, _, index2 = nb.nb_call(
            {Atom("path"): ["datetime", "datetime", "now"],
             Atom("args"): [],
             Atom("kwargs"): {},
             Atom("immediate"): False})
        print(nb.nb_retrieve(index2))

        sleep(0.5)
        _, _, index3 = nb.nb_call(
            {Atom("path"): [b"datetime", Atom("datetime"), b"now"],
             Atom("args"): [],
             Atom("kwargs"): {},
             Atom("immediate"): False})

        # Substitute $pyrlangval for first element of path, and for argument
        _, diff4 = nb.nb_call(
            {Atom("path"): [(Atom("$pyrlangval"), index3), "__sub__"],
             Atom("args"): [(Atom("$pyrlangval"), index2)],
             Atom("kwargs"): {},
             Atom("immediate"): True})
        print("diff ", diff4)

        nb.exit(Atom("normal"))


if __name__ == '__main__':
    unittest.main()

#!/usr/bin/env python3
from term import Atom
from pyrlang import Process, Node
from pyrlang import AsyncioEngine as Engine


class Receiver(Process):
    def __init__(self, node):
        Process.__init__(self, node.node_name_, passive=False)
        self.node = node
        self.node_name = self.node.node_name_.split("@")[0]
        self.node.register_name(self, Atom(self.node_name))
        print("Registering process - {0}".format(self.node.node_name_))

    def handle_one_inbox_message(self, msg):
        print("Received", msg)

    def process_loop(self) -> bool:
        """ Returns True to continue running. False to stop. """
        self.handle_inbox()

        return not self.is_exiting_


event_engine = Engine()
node = Node(node_name="receiver@127.0.0.1",
            cookie="COOKIE",
            engine=event_engine)
receiver = Receiver(node)
event_engine.run_forever()

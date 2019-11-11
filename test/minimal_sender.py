#!/usr/bin/env python3
from term import Atom
from pyrlang import Process, Node
from pyrlang import AsyncioEngine as Engine


class Sender(Process):
    def __init__(self, node):
        Process.__init__(self, node.node_name_, passive=False)
        self.node = node
        self.node_name = self.node.node_name_.split("@")[0]
        self.node.register_name(self, Atom(self.node_name))
        self.i = 0
        print("Registering process - {0}".format(self.node.node_name_))

    def handle_one_inbox_message(self, msg):
        print("Received", msg)

    def process_loop(self) -> bool:
        """ Returns True to continue running. False to stop. """
        self.handle_inbox()
        if self.i == 30:
            self.node.send(sender=self.pid_,
                           receiver=(Atom('receiver@127.0.0.1'), Atom('receiver')),
                           message=Atom('test'))
            self.i = 0
        self.i += 1
        return not self.is_exiting_


def my_process_clock(receiver):
    print(".")
    return True


event_engine = Engine()
node = Node(node_name="sender@127.0.0.1",
            cookie="COOKIE",
            engine=event_engine)

# fake_pid = node.register_new_process()
# node.send(sender=fake_pid,
#           receiver=(Atom('receiver@127.0.0.1'), Atom('fake')),
#           message=Atom('fake'))
# event_engine.sleep(1)

receiver = Sender(node)
event_engine.spawn(lambda: my_process_clock(receiver))

event_engine.run_forever()

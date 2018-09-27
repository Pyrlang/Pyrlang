# Python -> link -> Erlang
#
# This example shows:
# 1. Linking to an Erlang process from Python and killing it remotely
# 2. An exit message will be delivered to Pyrlang PID which was linked to it.
#
# Run: `make example6a` in one terminal window, then `make example6b` in another
#

import logging

from term import Atom
from pyrlang import Node, Process
# from pyrlang import GeventEngine as Engine
from pyrlang import AsyncioEngine as Engine
from colors import color

LOG = logging.getLogger(color("EXAMPLE6", fg='lime'))
logging.getLogger("").setLevel(logging.DEBUG)


class LinkExample6(Process):
    def __init__(self, node) -> None:
        Process.__init__(self, node_name=node.node_name_)

    def handle_one_inbox_message(self, msg):
        #
        # 1.1. Erlang node spawned a process for us and replied with a Pid
        #
        if isinstance(msg, tuple) and msg[0] == Atom("test_link"):
            LOG.info("LinkExample6: Linking to %s and killing", msg)
            n = self.get_node()
            n.link(self.pid_, msg[1])

            def exit_fn():
                n.exit_process(sender=self.pid_, receiver=msg[1],
                               reason=Atom("example6_link_exit"))
            self.engine_.call_later(0.5, exit_fn)
        else:
            LOG.info("LinkExample6: Incoming %s", msg)

    def exit(self, reason=None):
        #
        # 1.2. Exiting remote linked process should also exit this process
        #
        LOG.info("LinkExample6: Received EXIT(%s)" % reason)
        Process.exit(self, reason)

def main():
    event_engine = Engine()
    node = Node(node_name="py@127.0.0.1", cookie="COOKIE", engine=event_engine)

    #
    # 1. Create a process P1
    #   Send a message to process example6 on the Erlang node with "test_link"
    #   command. This will spawn an Erlang process and tell us the pid.
    #   Reply from Erlang node will trigger next steps above in ExampleProcess6
    #
    p1 = LinkExample6(node)

    LOG.info("Sending {example6, test_link, %s} to remote 'example6'" % p1.pid_)
    remote_receiver_name = (Atom('erl@127.0.0.1'), Atom("example6"))
    node.send(sender=p1.pid_,
              receiver=remote_receiver_name,
              message=(Atom("example6"), Atom("test_link"), p1.pid_))

    sleep_sec = 5
    LOG.info("Sleep %d sec" % sleep_sec)
    event_engine.sleep(sleep_sec)

    #
    # 3. End, sending a stop message
    #
    LOG.info(color("Stopping remote loop", fg="red"))
    node.send(sender=p1.pid_, receiver=remote_receiver_name,
              message=(Atom("example6"), Atom("stop")))

    event_engine.sleep(sleep_sec)
    node.destroy()
    LOG.error("Done")


if __name__ == "__main__":
    main()

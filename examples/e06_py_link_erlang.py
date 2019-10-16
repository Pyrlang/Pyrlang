# Python -> link -> Erlang
#
# This example shows:
# 1. Linking to an Erlang process from Python and killing it remotely
# 2. An exit message will be delivered to Pyrlang PID which was linked to it.
#
# Run: `make example6a` in one terminal window, then `make example6b` in another
#

import logging
import asyncio

from term import Atom
from pyrlang2.node import Node
from pyrlang2.process import Process
from colors import color

LOG = logging.getLogger(color("EXAMPLE6", fg='lime'))
logging.getLogger("").setLevel(logging.DEBUG)


class LinkExample6(Process):
    def __init__(self) -> None:
        Process.__init__(self)

    def handle_one_inbox_message(self, msg):
        #
        # 1.1. Erlang node spawned a process for us and replied with a Pid
        #
        if isinstance(msg, tuple) and msg[0] == Atom("test_link"):
            LOG.info("LinkExample6: Linking to %s and killing", msg)
            n = self.get_node()
            n.link_cast(self.pid_, msg[1])

            def exit_fn():
                n.exit_process(sender=self.pid_, receiver=msg[1],
                               reason=Atom("example6_link_exit"))
            asyncio.get_running_loop().call_later(0.5, exit_fn)
        else:
            LOG.info("LinkExample6: Incoming %s", msg)

    def exit(self, reason=None):
        #
        # 1.2. Exiting remote linked process should also exit this process
        #
        LOG.info("LinkExample6: Received EXIT(%s)" % reason)
        Process.exit(self, reason)

def main():
    event_engine = asyncio.get_event_loop()
    node = Node(node_name="py@127.0.0.1", cookie="COOKIE")

    #
    # 1. Create a process P1
    #   Send a message to process example6 on the Erlang node with "test_link"
    #   command. This will spawn an Erlang process and tell us the pid.
    #   Reply from Erlang node will trigger next steps above in ExampleProcess6
    #
    p1 = LinkExample6()

    LOG.info("Sending {example6, test_link, %s} to remote 'example6'" % p1.pid_)
    remote_receiver_name = (Atom('erl@127.0.0.1'), Atom("example6"))
    send_task = lambda: node.send(sender=p1.pid_,
                                  receiver=remote_receiver_name,
                                  message=(Atom("example6"),
                                           Atom("test_link"),
                                           p1.pid_))

    sleep_sec = 5
    LOG.info("Sleep %d sec" % sleep_sec)

    #
    # 3. End, sending a stop message
    #
    def task_sleep1():
        LOG.info(color("Stopping remote loop", fg="red"))
        node.send(sender=p1.pid_,
                  receiver=remote_receiver_name,
                  message=(Atom("example6"), Atom("stop")))

    def task_sleep2():
        node.destroy()
        LOG.error("Done")

    event_engine.call_soon(send_task)
    event_engine.call_later(sleep_sec, task_sleep1)
    event_engine.call_later(2 * sleep_sec, task_sleep2)
    event_engine.run_forever()


if __name__ == "__main__":
    main()

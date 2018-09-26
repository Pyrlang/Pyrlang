# Erlang -> link/monitor -> Python
#
# This example shows:
# 1. Remotely killing a Pyrlang process, and a Erlang process
# 2. Linking from Erlang to a Pyrlang process (and killing via the link)
# 3. Monitoring from Erlang a Pyrlang process and observing it exit
#
# Run: `make example5a` in one terminal window, then `make example5b` in another
#

import logging

from term import Atom
from pyrlang import Node, Process
# from pyrlang import GeventEngine as Engine
from pyrlang import AsyncioEngine as Engine
from colors import color

LOG = logging.getLogger(color("EXAMPLE5", fg='lime'))
logging.getLogger("").setLevel(logging.DEBUG)


class ExampleProcess5(Process):
    def __init__(self, node) -> None:
        Process.__init__(self, node_name=node.node_name_)

    def handle_one_inbox_message(self, msg):
        LOG.info("Process5: Incoming %s", msg)

    def exit(self, reason=None):
        LOG.info("Process5: Received EXIT(%s) from remote" % reason)
        Process.exit(self, reason)


def main():
    event_engine = Engine()
    node = Node(node_name="py@127.0.0.1", cookie="COOKIE", engine=event_engine)

    #
    # 1. Create a process P1
    #   Send pid of P1 to process example5 on the Erlang node with "test_exit"
    #   command. This will trigger exit signal to P1 from Erlang.
    #
    p1 = ExampleProcess5(node)

    LOG.info("Sending {example5, test_exit, %s} to remote 'example5'" % p1.pid_)
    remote_receiver_name = (Atom('erl@127.0.0.1'), Atom("example5"))
    node.send(sender=p1.pid_,
              receiver=remote_receiver_name,
              message=(Atom("example5"), Atom("test_exit"), p1.pid_))

    event_engine.sleep(1)
    assert p1.is_exiting_  # assume it exited
    del p1

    sleep_sec = 3
    LOG.info("Sleep %d sec" % sleep_sec)
    event_engine.sleep(sleep_sec)

    #
    # 2. Create a process P2
    #   Send pid of P2 to process example5 on the Erlang node with "test_link"
    #   command, it will link remotely and try to kill us and observe the
    #   results (exit signal will be returned to Erlang).
    #
    p2 = ExampleProcess5(node)
    LOG.info("Sending {example5, test_link, %s} to remote 'example5'" % p2.pid_)
    node.send(sender=p2.pid_, receiver=remote_receiver_name,
              message=(Atom("example5"), Atom("test_link"), p2.pid_))

    LOG.info("Sleep %d sec" % sleep_sec)
    event_engine.sleep(sleep_sec)

    #
    # 3. Create a process P3
    #   Send pid of P3 to remote process example5 and it will monitor P3 and
    #   then will send an exit signal.
    #
    p3 = ExampleProcess5(node)
    LOG.info("Sending {example5, test_monitor, %s} to remote 'example5'" % p3.pid_)
    node.send(sender=p3.pid_, receiver=remote_receiver_name,
              message=(Atom("example5"), Atom("test_monitor"), p3.pid_))

    LOG.info("Sleep %d sec" % sleep_sec)
    event_engine.sleep(sleep_sec)

    #
    # 4. End, sending a stop message
    #
    LOG.info("Stopping remote loop")
    node.send(sender=p2.pid_, receiver=remote_receiver_name,
              message=(Atom("example5"), Atom("stop")))

    event_engine.sleep(sleep_sec)
    LOG.error("Done")


if __name__ == "__main__":
    main()

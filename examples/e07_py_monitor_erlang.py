# Python -> monitor -> Erlang
#
# This example shows:
# 1. Monitoring an Erlang process from Python and killing it remotely
# 2. A ``{'DOWN', Ref, 'process', Pid, Reason}`` message will be delivered to
#      the Pyrlang process which was monitoring the Erlang process.
#
# Run: `make example7a` in one terminal window, then `make example7b` in another
#

import logging

from term import Atom
from pyrlang import Node, Process
# from pyrlang import GeventEngine as Engine
from pyrlang import AsyncioEngine as Engine
from colors import color

LOG = logging.getLogger(color("EXAMPLE7", fg='lime'))
logging.getLogger("").setLevel(logging.DEBUG)


class MonitorExample7(Process):
    def __init__(self) -> None:
        Process.__init__(self)

    def handle_one_inbox_message(self, msg):
        #
        # 1.1. Erlang node spawned a process for us and replied with a Pid
        #
        if isinstance(msg, tuple) and msg[0] == Atom("test_monitor"):
            LOG.info("MonitorExample7: Monitoring %s and killing", msg)
            n = self.get_node()
            n.monitor_process(self.pid_, msg[1])

            def exit_fn():
                n.exit_process(sender=self.pid_, receiver=msg[1],
                               reason=Atom("example7_monitor_exit"))
            self.engine_.call_later(0.5, exit_fn)
        else:
            LOG.info("MonitorExample7: Incoming %s", msg)

    def exit(self, reason=None):
        #
        # 1.2. Exiting remote linked process should also exit this process
        #
        LOG.info("MonitorExample7: Received EXIT(%s)" % reason)
        Process.exit(self, reason)


def main():
    event_engine = Engine()
    node = Node(node_name="py@127.0.0.1", cookie="COOKIE", engine=event_engine)

    #
    # 1. At the same time as P1 (they should not interfere) create a process P2
    #   Send a message to process example7 on the Erlang node with "test_monitor"
    #   command. This will spawn an Erlang process and tell us the pid.
    #   Reply from Erlang node will trigger next steps above in ExampleProcess6
    #
    proc = MonitorExample7()

    LOG.info("Sending {example7, test_monitor, %s} to remote 'example7'" % proc.pid_)
    remote_receiver_name = (Atom('erl@127.0.0.1'), Atom("example7"))
    node.send(sender=proc.pid_,
              receiver=remote_receiver_name,
              message=(Atom("example7"), Atom("test_monitor"), proc.pid_))

    sleep_sec = 5
    LOG.info("Sleep %d sec" % sleep_sec)
    event_engine.sleep(sleep_sec)

    #
    # 3. End, sending a stop message
    #
    LOG.info(color("Stopping remote loop", fg="red"))
    node.send(sender=proc.pid_, receiver=remote_receiver_name,
              message=(Atom("example7"), Atom("stop")))

    event_engine.sleep(sleep_sec)
    node.destroy()
    LOG.error("Done")


if __name__ == "__main__":
    main()

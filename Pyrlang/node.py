from __future__ import print_function

import gevent
import time
from gevent import Greenlet

from Pyrlang.Dist.distribution import ErlDistribution
from Pyrlang.atom import ErlAtom
from Pyrlang.process import ErlProcess

PID_MARKER = "pyrlang.Pid"


class ErlPid:
    """ Process identifier implementation    
    """

    def __init__(self, p) -> None:
        self.pid = p

    def equals(self, other) -> bool:
        return isinstance(other, ErlPid) and self.pid == other.pid

    __eq__ = equals

    def __ne__(self, other):
        return not self.equals(other)

    def __hash__(self):
        return hash((PID_MARKER, self.pid))


class ErlNode(Greenlet):
    """ Implements an Erlang node which has a network name, a dictionary of 
        processes and registers itself via EPMD. 
        Node handles the networking asynchronously.
        
        Usage example:
        1. Create a node class with a name and a cookie
        2. Give it time periodically to poll the sockets with node.poll(), 
            or call node.infinite_loop()
    """

    def __init__(self, name: str, cookie: str, *args, **kwargs) -> None:
        Greenlet.__init__(self)

        self.pid_counter = 0
        self.processes = {}
        self.registered_names = {}
        self.exiting = False

        self.dist = ErlDistribution(self, name, cookie)

    def _run(self):
        while not self.exiting:
            self.dist.connect()
            gevent.sleep(5)

    def register_new_process(self, proc) -> ErlPid:
        """ Generate a new pid and add the process to the process dictionary        
            :param proc: A new born process 
            :return: Pid for it (does not modify the process in place!)
        """
        pid = ErlPid(self.pid_counter)
        self.pid_counter += 1
        self.processes[pid] = proc
        return pid

    def register_name(self, proc: ErlProcess, name: ErlAtom) -> None:
        """ Add a name into registrations table (automatically removed when the
            referenced process is removed)
            :param proc: The process to register 
            :param name: The name to register with
        """
        self.pid_counter += 1
        self.registered_names[proc.pid] = proc

    def stop(self) -> None:
        """ Sets the mark that node is done, closes connections and leaves
            the infinite_loop, if we were in it.
        """
        self.exiting = True
        self.dist.disconnect()

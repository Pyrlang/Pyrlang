from __future__ import print_function

import gevent
from gevent import Greenlet

from Pyrlang.Dist.distribution import ErlDistribution
from Pyrlang.Dist.node_opts import ErlNodeOpts
from Pyrlang.atom import ErlAtom
from Pyrlang.process import ErlProcess
from Pyrlang.pid import ErlPid


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

        self.pid_counter_ = 0
        self.processes_ = {}
        self.reg_names_ = {}
        self.is_exiting_ = False
        self.node_opts_ = ErlNodeOpts(cookie=cookie)

        self.dist_ = ErlDistribution(node=self, name=name)

    def _run(self):
        self.dist_.connect(self)

        while not self.is_exiting_:
            gevent.sleep(5)

    def register_new_process(self, proc) -> ErlPid:
        """ Generate a new pid and add the process to the process dictionary        
            :param proc: A new born process 
            :return: Pid for it (does not modify the process in place!)
        """
        pid = ErlPid(self.pid_counter_)
        self.pid_counter_ += 1
        self.processes_[pid] = proc
        return pid

    def register_name(self, proc: ErlProcess, name: ErlAtom) -> None:
        """ Add a name into registrations table (automatically removed when the
            referenced process is removed)
            :param proc: The process to register 
            :param name: The name to register with
        """
        self.pid_counter_ += 1
        self.reg_names_[proc.pid] = proc

    def stop(self) -> None:
        """ Sets the mark that node is done, closes connections and leaves
            the infinite_loop, if we were in it.
        """
        self.is_exiting_ = True
        self.dist_.disconnect()

__all__ = ['ErlNode']

from __future__ import print_function

import gevent
from gevent import Greenlet

from Pyrlang import term
from Pyrlang.Dist.distribution import ErlDistribution
from Pyrlang.Dist.node_opts import ErlNodeOpts
from Pyrlang.atom import ErlAtom
from Pyrlang.pid import ErlPid
from Pyrlang.process import ErlProcess


class ErlNodeException(Exception):
    pass


class ErlNode(Greenlet):
    """ Implements an Erlang node which has a network name, a dictionary of 
        processes and registers itself via EPMD. 
        Node handles the networking asynchronously.
        
        Usage example:
        1. Create a node class with a name and a cookie
        2. Give it time periodically to poll the sockets with node.poll(), 
            or call node.infinite_loop()
    """
    singleton = None

    def __init__(self, name: str, cookie: str, *args, **kwargs) -> None:
        Greenlet.__init__(self)

        if ErlNode.singleton is not None:
            raise ErlNodeException("Singleton ErlNode already created")
        ErlNode.singleton = self

        self.pid_counter_ = 0
        self.processes_ = {}
        self.reg_names_ = {}
        self.is_exiting_ = False
        self.node_opts_ = ErlNodeOpts(cookie=cookie)
        self.name_ = term.Atom(name)

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

    def register_name(self, proc: ErlProcess, name: term.Atom) -> None:
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

    def registered_send(self, sender, receiver, message):
        """ Send a message to a named process
            :param sender: A pid who sent the message
            :param receiver: A name, atom, of the receiver process
            :param message: The message
        """
        if not isinstance(receiver, term.Atom):
            raise ErlNodeException("registered_send receiver must be an atom")

        if receiver.text_ == 'net_kernel':
            return self.handle_net_kernel_message(message)

        print("regsend %s: %s" % (receiver, message))

    def handle_net_kernel_message(self, m):
        if not isinstance(m[0], term.Atom):
            return

        if m[0].text_ == '$gen_call':
            (sender, ref) = m[1]
            msg = m[2]

            if not isinstance(msg[0], term.Atom):
                return

            if msg[0].text_ == 'is_auth':
                # other_node = msg[1]
                self.send(sender, (ref, term.Atom('yes')))

    def send(self, receiver, message):
        if not isinstance(receiver, term.Pid) \
                and receiver not in self.reg_names_:
            raise ErlNodeException("Receiver must be a pid or a registered name")

        if self.name_ == receiver.node_:
            # TODO: Local send
            pass

        else:
            # TODO: Remote send
            print("remote to %s: %s" % (receiver, message))


__all__ = ['ErlNode']

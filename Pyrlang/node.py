from __future__ import print_function

import gevent
from gevent import Greenlet
from gevent.queue import Queue

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

        self.inbox_ = Queue()
        self.pid_counter_ = 0
        self.processes_ = {}
        self.reg_names_ = {}
        self.is_exiting_ = False
        self.node_opts_ = ErlNodeOpts(cookie=cookie)
        self.name_ = term.Atom(name)

        self.dist_nodes_ = {}
        self.dist_ = ErlDistribution(node=self, name=name)

    def _run(self):
        self.dist_.connect(self)

        while not self.is_exiting_:
            while not self.inbox_.empty():
                msg = self.inbox_.get_nowait()
                self.handle_one_inbox_message(msg)
            gevent.sleep(0)

    def handle_one_inbox_message(self, m):
        # Send a ('node_connected', IP, Connection) to inform about the
        # connectivity with the other node
        if m[0] == 'node_connected':
            (_, addr, connection) = m
            self.dist_nodes_[addr] = connection

        # Send a ('node_disconnected', IP) to forget the connection
        if m[0] == 'node_disconnected':
            (_, addr) = m
            del self.dist_nodes_[addr]

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

    def _registered_send(self, receiver, message):
        """ Try find a named process by atom key, drop a message into its inbox_
            :param receiver: A name, atom, of the receiver process
            :param message: The message
        """
        if not isinstance(receiver, term.Atom):
            raise ErlNodeException("registered_send receiver must be an atom")

        if receiver.text_ == 'net_kernel':
            return self.handle_net_kernel_message(message)

        if receiver in self.reg_names_:
            dst = self.reg_names_[receiver]
            dst.inbox_.put(message)

    def _send_local(self, receiver, message):
        """ Try find a process by pid and drop a message into its inbox_
            :param receiver:  Pid who will receive the message
            :param message:  The message
        """
        if not isinstance(receiver, term.Pid):
            raise ErlNodeException("send's receiver must be a pid")

        if receiver in self.processes_:
            dst = self.processes_[receiver]
            dst.inbox_.put(message)

    def handle_net_kernel_message(self, m):
        """ Net_kernel is the registered process in Erlang responsible for
            net_adm:ping's for example. So we can satisfy this here for our node
            to become pingable
        """
        if not isinstance(m[0], term.Atom):
            return

        # Incoming gen_call packet to net_kernel, might be that net_adm:ping
        if m[0].text_ == '$gen_call':
            (sender, ref) = m[1]
            msg = m[2]

            if not isinstance(msg[0], term.Atom):
                return

            if msg[0].text_ == 'is_auth':
                # other_node = msg[1]
                # Respond with {Ref, 'yes'}
                self._send_remote(sender, (ref, term.Atom('yes')))

    def send(self, sender, receiver, message):
        is_atom = isinstance(receiver, term.Atom)
        if is_atom:
            return self._registered_send(receiver, message)

        is_pid = isinstance(receiver, term.Pid)
        if is_pid and self.name_ == receiver.node_:
            return self._send_local(receiver, message)
        else:
            return self._send_remote(receiver, message)

    def _send_remote(self, receiver, message):
        receiver_node = receiver.node_.text_
        if receiver_node not in self.dist_nodes_:
            raise ErlNodeException("Node not connected %s" % receiver_node)

        conn = self.dist_nodes_[receiver_node]
        conn.inbox_.put(('send', receiver, message))
        print("remote to %s: %s" % (receiver, message))


__all__ = ['ErlNode']

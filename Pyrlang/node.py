from __future__ import print_function

from typing import Union

import gevent
from gevent import Greenlet
from gevent.queue import Queue

from Pyrlang import term, logger
from Pyrlang.Dist.distribution import ErlangDistribution
from Pyrlang.Dist.node_opts import NodeOpts
from Pyrlang.process import Process

LOG = logger.nothing
WARN = logger.nothing
ERROR = logger.tty


class NodeException(Exception):
    pass


class Node(Greenlet):
    """ Implements an Erlang node which has a network name, a dictionary of 
        processes and registers itself via EPMD. 
        Node handles the networking asynchronously.
        
        Usage example:
        1. Create a node class with a name and a cookie
        2. Give it time periodically to poll the sockets with node.poll(), 
            or call node.infinite_loop()
    """
    singleton = None

    def __init__(self, name: str, cookie: str) -> None:
        Greenlet.__init__(self)

        if Node.singleton is not None:
            raise NodeException("Singleton Node was already created")
        Node.singleton = self

        self.inbox_ = Queue()
        self.pid_counter_ = 0
        self.processes_ = {}
        self.reg_names_ = {}
        self.is_exiting_ = False
        self.node_opts_ = NodeOpts(cookie=cookie)
        self.name_ = term.Atom(name)

        self.dist_nodes_ = {}
        self.dist_ = ErlangDistribution(node=self, name=name)

        # Spawn and register (automatically) the process 'rex' for remote
        # execution, which takes 'rpc:call's from Erlang
        from Pyrlang.rex import Rex
        Rex(self).start()

        # Spawn and register (automatically) the 'net_kernel' process which
        # handles special ping messages
        from Pyrlang.net_kernel import NetKernel
        NetKernel(self).start()

    def _run(self):
        self.dist_.connect(self)

        while not self.is_exiting_:
            while not self.inbox_.empty():
                msg = self.inbox_.get_nowait()
                self.handle_one_inbox_message(msg)
            gevent.sleep(0.0)

    def handle_one_inbox_message(self, m):
        # Send a ('node_connected', IP, Connection) to inform about the
        # connectivity with the other node
        if m[0] == 'node_connected':
            (_, addr, connection) = m
            self.dist_nodes_[addr] = connection

        # Send a ('node_disconnected', IP) to forget the connection
        elif m[0] == 'node_disconnected':
            (_, addr) = m
            del self.dist_nodes_[addr]

    def register_new_process(self, proc) -> term.Pid:
        """ Generate a new pid and add the process to the process dictionary        
            :param proc: A new born process 
            :return: Pid for it (does not modify the process in place!)
        """
        pid = term.Pid(node=self.name_,
                       id=self.pid_counter_ // 0x7fffffff,
                       serial=self.pid_counter_ % 0x7fffffff,
                       creation=self.dist_.creation_)
        self.pid_counter_ += 1
        self.processes_[pid] = proc
        return pid

    def register_name(self, proc: Process, name: term.Atom) -> None:
        """ Add a name into registrations table (automatically removed when the
            referenced process is removed)
            :param proc: The process to register 
            :param name: The name to register with
        """
        self.reg_names_[name] = proc.pid_

    def stop(self) -> None:
        """ Sets the mark that node is done, closes connections and leaves
            the infinite_loop, if we were in it.
        """
        self.is_exiting_ = True
        self.dist_.disconnect()

    def where_is(self, ident) -> Union[Process, None]:
        if isinstance(ident, term.Atom) and ident in self.reg_names_:
            ident = self.reg_names_[ident]

        if isinstance(ident, term.Pid) and ident in self.processes_:
            return self.processes_[ident]

        return None

    def _send_local_registered(self, receiver, message):
        """ Try find a named process by atom key, drop a message into its inbox_
            :param receiver: A name, atom, of the receiver process
            :param message: The message
        """
        if not isinstance(receiver, term.Atom):
            raise NodeException("_send_local_registered receiver must be an "
                                "atom")

        receiver_obj = self.where_is(receiver)
        if receiver_obj is not None:
            receiver_obj.inbox_.put(message)
        else:
            WARN("Node: send to unregistered name %s ignored" % receiver)

    def _send_local(self, receiver, message):
        """ Try find a process by pid and drop a message into its inbox_
            :param receiver:  Pid who will receive the message
            :param message:  The message
        """
        if not isinstance(receiver, term.Pid):
            raise NodeException("send's receiver must be a pid")

        dst = self.where_is(receiver)
        if dst is not None:
            LOG("Node._send_local: pid %s <- %s" % (receiver, message))
            dst.inbox_.put(message)
        else:
            WARN("Node._send_local: pid %s does not exist" % receiver)

    def send(self, sender: term.Pid,
             receiver: Union[term.Pid, term.Atom],
             message):
        """ Determines whether receiver is an atom or a pid, and whether it is
            a local or remote pid, then delivers it using different functions
        """
        LOG("send to %s: %s" % (receiver, message))

        if isinstance(receiver, term.Pid):
            if self.name_ == receiver.node_:
                return self._send_local(receiver, message)
            else:
                return self._send_remote(receiver, message)

        if isinstance(receiver, term.Atom):
            return self._send_local_registered(receiver, message)

        raise NodeException("Don't know how to send to %s" % receiver)

    def _send_remote(self, receiver, message):
        LOG("Node._send_remote %s <- %s" % (receiver, message))
        m = ('send', receiver, message)
        return self.dist_command(receiver_node=receiver.node_.text_,
                                 message=m)

    def dist_command(self, receiver_node: str, message):
        """ Place a tuple crafted by the caller into message box for Erlang
            distribution socket. It will handle the message with a
            'handle_one_inbox_message' call.

        :param receiver_node: Name of the node whose connection is affected
        :param message: A crafted tuple with some values
        """
        if receiver_node not in self.dist_nodes_:
            raise NodeException("Node not connected %s" % receiver_node)

        conn = self.dist_nodes_[receiver_node]
        conn.inbox_.put(message)

    def monitor_process(self, origin: term.Pid, target):
        target_proc = self.where_is(target)
        LOG("MonitorP: org %s targ %s = %s" % (origin, target, target_proc))
        if target_proc is not None:
            target_proc.monitors_.add(origin)
        else:
            msg = "Monitor target %s does not exist" % target
            raise NodeException(msg)

    def demonitor_process(self, origin, target):
        target_proc = self.where_is(target)
        if target_proc is not None:
            target_proc.monitors_.discard(origin)
        else:
            msg = "Demonitor target %s does not exist" % target
            raise NodeException(msg)


__all__ = ['Node']

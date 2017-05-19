from __future__ import print_function
import gevent
from gevent.server import StreamServer

from Pyrlang.Dist import util
from Pyrlang.Dist.epmd import ErlEpmd
from Pyrlang.Dist.in_connection import *


class ErlDistribution:
    """ Implements network part of the EPMD registration and Erlang distribution
        protocol. Extends functionality of ErlNode, so all functions take 
        ErlNode as a parameter but don't store it to avoid creating a ref cycle    
    """

    def __init__(self, node, name: str) -> None:
        self.name_ = name
        self.creation_ = None

        # Listener for Incoming connections from other nodes
        # Create handler using make_handler helper
        proto_kwargs = {"node_opts": node.node_opts_,
                        "dist": self}
        handler = util.make_handler(receiver_class=InConnection,
                                    args=[],
                                    kwargs=proto_kwargs)

        self.in_srv_ = StreamServer(listener=('127.0.0.1', 0),
                                    handle=handler)
        self.in_srv_.start()
        self.in_port_ = self.in_srv_.server_port
        print("Dist: Listening for dist connections on port", self.in_port_)

        self.epmd_ = ErlEpmd()

    def connect(self, node) -> bool:
        """ Looks up EPMD daemon and connects to it trying to discover other 
            Erlang nodes.
        """
        while True:
            if self.epmd_.connect():
                return self.epmd_.alive2(self)

            gevent.sleep(5)

    def disconnect(self):
        """
        :return:
        """
        self.epmd_.close()


__all__ = ['ErlDistribution']

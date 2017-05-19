class InConnection:
    """ Handling incoming connections from other nodes
    """
    def on_connected(self, sockt, address):
        pass

    def on_packet(self, packet):
        pass

    def on_connection_lost(self):
        pass


__all__ = ['InConnection']

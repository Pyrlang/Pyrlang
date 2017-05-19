import struct
import gevent
import sys
from gevent import socket

NODE_HIDDEN = 77
NODE_NORMAL = 72
DIST_VSN = 5  # Distribution protocol version

REQ_ALIVE2 = 120
DIST_PROTOCOL = 0  # 0 = TCP/IP

PY3 = sys.version_info[0] >= 3


class ErlEpmd:
    """ Represents an EPMD client
    """

    def __init__(self) -> None:
        self.host_ = '127.0.0.1'
        self.port_ = 4369

        self.sock_ = None  # network socket

    def close(self):
        print("EPMD: Close")
        self.sock_.close()
        self.sock_ = None

    def connect(self) -> bool:
        """ A long running connection to EPMD
            :param dist: Distribution object
            :return: True
        """
        while True:
            try:
                print("EPMD: Connecting %s:%d" % (self.host_, self.port_))
                host_port = (self.host_, self.port_)
                self.sock_ = socket.create_connection(address=host_port,
                                                      timeout=5.0)
                break  # the connect loop

            except socket.error as err:
                print("EPMD: connection error:", err)
                gevent.sleep(5)

        print("EPMD: Socket connected")
        return True

    def alive2(self, dist) -> bool:
        # Say hello to our EPMD friend
        self._req_alive2(nodetype=NODE_HIDDEN,
                         node_name=dist.name_,
                         in_port=dist.in_port_,
                         dist_vsn=(DIST_VSN, DIST_VSN),
                         extra="")

        creation = self._read_alive2_reply()
        if creation >= 0:
            print("EPMD: Connected successfully (creation %d)"
                  % creation)
            dist.creation_ = creation
            return True

        print("EPMD: ALIVE2 failed with creation %d" % creation)
        return False

    def _read_alive2_reply(self) -> int:
        """ Read reply from ALIVE2 request, check the result code, read creation
            :return: Creation value if all is well, connection remains on.
                On error returns -1
        """
        # Reply will be [121,0,Creation:16] for OK, otherwise [121,Error]
        reply = self.sock_.recv(2)
        if not reply:
            print("EPMD: ALIVE2 Read error. Closed?", reply)
            return -1

        if reply[1] == 0:
            cr = self.sock_.recv(2)
            (creation,) = struct.unpack(">H", cr)
            return creation

        print("EPMD: ALIVE2 returned error", reply[1])
        return -1

    @staticmethod
    def _make_req_alive2(nodetype: int, name: str, in_port: int,
                         dist_vsn: tuple, extra: str):
        extra = bytes(extra, "latin1")
        name = bytes(name.split("@")[0], "utf8")

        msg1 = struct.pack(">BH BB HH",
                           REQ_ALIVE2, in_port,
                           nodetype, DIST_PROTOCOL,
                           dist_vsn[0], dist_vsn[1])
        msg2 = struct.pack(">H", len(name)) + name
        msg3 = struct.pack(">H", len(extra)) + extra

        return msg1 + msg2 + msg3

    def _req_alive2(self, nodetype: int, node_name: str, in_port: int,
                    dist_vsn: tuple, extra: str):
        msg = self._make_req_alive2(nodetype, node_name, in_port,
                                    dist_vsn, extra)
        print("EPMD: sending ALIVE2 req", (node_name, nodetype, dist_vsn))
        self._req(msg)

    def _req(self, req: bytes):
        """ Generic helper function to send a preformatted request to EPMD
        """
        header = struct.pack(">H", len(req))
        self.sock_.sendall(header + req)


__all__ = ['ErlEpmd']

from __future__ import absolute_import
from __future__ import print_function

from Pyrlang import term, gen
from Pyrlang.process import Process
from Pyrlang.node import Node


class NetKernel(Process):
    """ A special process which registers itself as ``net_kernel`` and handles
        one specific ``is_auth`` message, which is used by ``net_adm:ping``.
    """

    def __init__(self, node: Node) -> None:
        Process.__init__(self, node)
        node.register_name(self, term.Atom('net_kernel'))

    def handle_one_inbox_message(self, msg):
        gencall = gen.parse_gen_message(msg)
        if not isinstance(gencall, gen.GenIncomingMessage):
            print("NetKernel:", gencall)
            return

        # Incoming gen_call packet to net_kernel, might be that net_adm:ping
        msg = gencall.message_

        if isinstance(msg[0], term.Atom) and msg[0].text_ == 'is_auth':
            # Respond with {Ref, 'yes'}
            gencall.reply(local_pid=self.pid_, result=term.Atom('yes'))
        else:
            print("NetKernel: unknown message", msg)


__all__ = ['NetKernel']

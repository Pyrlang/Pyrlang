from __future__ import absolute_import
from __future__ import print_function

from Pyrlang import term, gen
from Pyrlang.process import Process
from Pyrlang.node import Node


class Rex(Process):
    """ Remote executor for RPC calls. Registers itself under the name 'rex' and
        accepts RPC call messages.
        Erlang 'rpc:call' sends a gen:call message to name 'rex' which we parse
    """

    def __init__(self, node: Node) -> None:
        Process.__init__(self, node)
        node.register_name(self, term.Atom('rex'))

    def handle_one_inbox_message(self, msg):
        # TODO: Factor this out into a gen_call library, also in Node.net_kernel

        gencall = gen.parse_gen_call(msg)
        if isinstance(gencall, str):
            print("REX:", gencall)
            return

        # Find and run the thing
        pmod = __import__(gencall.get_mod_str(), fromlist=[''])
        pfun = getattr(pmod, gencall.get_fun_str())

        args = gencall.get_args()
        try:
            result = pfun(*args)
            # Send a reply
            gencall.reply(local_pid=self.pid_,
                          message=(gencall.ref_, result))

        except Exception as excpt:
            # Send an error
            gencall.reply_exit(local_pid=self.pid_,
                               reason=excpt)


__all__ = ['Rex']

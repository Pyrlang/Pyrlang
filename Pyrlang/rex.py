from __future__ import absolute_import
from __future__ import print_function

from Pyrlang import term
from Pyrlang.process import Process
from Pyrlang.node import Node


class Rex(Process):
    """ Remote executor for RPC calls. Registers itself under the name 'rex' and
        accepts RPC call messages.
        Erlang 'rpc:call' sends a named message to 'rex' which we parse
    """

    def __init__(self, node: Node) -> None:
        Process.__init__(self, node)
        node.register_name(self, term.Atom('rex'))

    def handle_one_inbox_message(self, msg):
        # TODO: Factor this out into a gen_call library, also in Node.net_kernel

        # Incoming {$gen_call, {From, Ref}, {call, Mod, Fun, Args}}
        if type(msg) != tuple:  # ignore all non-tuple messages
            print("REX: Only {tuple} messages allowed")
            return

        # ignore tuples with non-atom 1st, ignore non-gen_call mesages
        if not isinstance(msg[0], term.Atom) or msg[0].text_ != '$gen_call':
            print("REX: Only {$gen_call, _, _} messages allowed")
            return

        (_, _sender_mref, _call_mfa_gl) = msg
        (msender, mref) = _sender_mref
        (call, m, f, args, group_leader) = _call_mfa_gl

        if not isinstance(call, term.Atom) or call.text_ != 'call':
            print("REX: Only {$gen_call, _, {call, ...}} requests are allowed")
            return

        # Find and run the thing
        print("REX: rpc call %s.%s %s" % (m, f, args))

        pmod = __import__(m.text_, fromlist=[''])
        pfun = getattr(pmod, f.text_)

        args = args.elements_
        try:
            result = pfun(*args)
            # Send a reply
            Node.singleton.send(sender=self.pid_,
                                receiver=msender,
                                message=(mref, result))

        except Exception as excpt:
            # Send an error
            reply = (term.Atom('DOWN'), mref, None, None, excpt)
            Node.singleton.send(sender=self.pid_,
                                receiver=msender,
                                message=reply)


def p(*args, **kwargs):
    print(*args, **kwargs)
    return (123, 1.23)
    #raise Exception("boom")


__all__ = ['Rex', 'p']

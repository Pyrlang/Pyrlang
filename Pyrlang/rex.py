from __future__ import absolute_import
from __future__ import print_function

from Pyrlang import term
from Pyrlang.process import Process
from Pyrlang.node import Node


class Rex(Process):
    """ Remote executor for RPC calls. Registers itself under the name 'rex' and
        accepts RPC call messages.
    """

    def __init__(self, node: Node) -> None:
        Process.__init__(self, node)
        node.register_name(self, term.Atom('rex'))

    def handle_one_inbox_message(self, msg):
        # Incoming {$gen_call, {From, Ref}, {call, Mod, Fun, Args}}
        if type(msg) != tuple:  # ignore all non-tuple messages
            print("REX: Only {tuple} messages allowed")
            return

        # ignore tuples with non-atom 1st, ignore non-gen_call mesages
        if not isinstance(msg[0], term.Atom) or msg[0].text_ != '$gen_call':
            print("REX: Only {$gen_call, _, _} messages allowed")
            return

        (_, from_ref, call_mfa) = msg
        (sender, ref) = from_ref
        (call, m, f, args, group_leader) = call_mfa

        if not isinstance(call, term.Atom) or call.text_ != 'call':
            print("REX: Only {$gen_call, _, {call, ...}} requests are allowed")
            return

        # Find and run the thing
        print("REX: rpc call %s.%s(%s)" % (m, f, args))

        pmod = __import__(m.text_, fromlist=[''])
        pfun = getattr(pmod, f.text_)

        args = args.elements_
        try:
            result = pfun(*args)
            # Send a reply
            Node.singleton.send(sender=self.pid_,
                                receiver=sender,
                                message=(ref, result))

        except Exception as e:
            # Send an error
            reply = (term.Atom('DOWN'), ref, None, None, str(e))
            Node.singleton.send(sender=self.pid_,
                                receiver=sender,
                                message=reply)


def p(*args, **kwargs):
    print(*args, **kwargs)


__all__ = ['Rex', 'p']

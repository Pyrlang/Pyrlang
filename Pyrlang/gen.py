#
# Assist with gen:call message handling
#

from Pyrlang import term


class GenBase:
    def __init__(self, sender, ref):
        self.sender_ = sender
        self.ref_ = ref

    def reply(self, local_pid, result):
        """ Reply with a gen:call result
        """
        from Pyrlang.node import Node
        Node.singleton.send(sender=local_pid,
                            receiver=self.sender_,
                            message=(self.ref_, result))

    def reply_exit(self, local_pid, reason):
        """ Reply to remote gen:call with EXIT message which causes reason to be
            re-raised as exit() on the caller side
            NOTE: The gen:call caller attempts to monitor the target first. If
                the monitor attempt fails, the exit here won't work
        """
        reply = (term.Atom('DOWN'), self.ref_, None, None, reason)
        from Pyrlang.node import Node
        Node.singleton.send(sender=local_pid,
                            receiver=self.sender_,
                            message=reply)


class GenIncomingMessage(GenBase):
    """ For those situations when gen message is not a call
    """
    def __init__(self, sender, ref, message):
        GenBase.__init__(self, sender=sender, ref=ref)
        self.message_ = message


class GenIncomingCall(GenBase):
    """ A helper class which parses incoming gen:call args and gives assistance
        with replying to the caller
    """

    def __init__(self, mod, fun, args, group_leader, sender, ref):
        GenBase.__init__(self, sender=sender, ref=ref)
        self.mod_ = mod
        self.fun_ = fun
        self.args_ = args
        self.group_leader_ = group_leader

    def get_args(self):
        return self.args_.elements_

    def get_mod_str(self):
        return self.mod_.text_

    def get_fun_str(self):
        return self.fun_.text_


def parse_gen_call(msg):
    """ Determine if msg is a gen:call message
        :param msg: An Erlang tuple hopefully starting with a '$gen_call'
        :return: str with error if msg isn't a call message, or parsed dict
            otherwise
    """
    # Incoming {$gen_call, {From, Ref}, {call, Mod, Fun, Args}}
    if type(msg) != tuple:  # ignore all non-tuple messages
        return "Only {tuple} messages allowed"

    # ignore tuples with non-atom 1st, ignore non-gen_call mesages
    if not isinstance(msg[0], term.Atom) or msg[0].text_ != '$gen_call':
        return "Only {$gen_call, _, _} messages allowed"

    (_, _sender_mref, _call_mfa_gl) = msg
    (msender, mref) = _sender_mref

    if len(_call_mfa_gl) != 5:
        return "Expecting a 5-tuple (with a 'call' atom)"
        # TODO: Maybe also check first element to be an atom 'call'

    # A gen_call call tuple has 5 elements, otherwise see below
    (call, m, f, args, group_leader) = _call_mfa_gl

    if not isinstance(m, term.Atom):
        return "Module must be an atom: %s" % str(m)

    if not isinstance(f, term.Atom):
        return "Function must be an atom: %s" % str(f)

    return GenIncomingCall(mod=m,
                           fun=f,
                           args=args,
                           group_leader=group_leader,
                           sender=msender,  # pid of the sender
                           ref=mref  # reference used in response
                           )


def parse_gen_message(msg):
    """ Might be an 'is_auth' request which is not a call
        :return string on error, otherwise a GenIncomingMessage object
    """
    # Incoming {$gen_call, {From, Ref}, Message}
    if type(msg) != tuple:  # ignore all non-tuple messages
        return "Only {tuple} messages allowed"

    # ignore tuples with non-atom 1st, ignore non-gen_call mesages
    if not isinstance(msg[0], term.Atom) or msg[0].text_ != '$gen_call':
        return "Only {$gen_call, _, _} messages allowed"

    (_, _sender_mref, gcmsg) = msg
    (msender, mref) = _sender_mref

    return GenIncomingMessage(sender=msender,
                              ref=mref,
                              message=gcmsg)


__all__ = ['GenIncomingCall', 'GenIncomingMessage',
           'parse_gen_call']

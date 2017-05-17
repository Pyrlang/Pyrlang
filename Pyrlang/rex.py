from __future__ import absolute_import
from Pyrlang.process import ErlProcess
from Pyrlang.node import ErlNode


class ErlRex(ErlProcess):
    """ Remote executor for RPC calls. Registers itself under the name 'rex' and
        accepts RPC call messages.
    """

    def __init__(self, node: ErlNode) -> None:
        ErlProcess.__init__(self, node)
        node.register_name(self, 'rex')

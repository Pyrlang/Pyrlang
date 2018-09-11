class BitString:
    """ A simple data holder to be able to pass bitstrings (bytes with
        incomplete last byte) back to Erlang/Elixir node.
    """
    def __init__(self, val: bytes, last_byte_bits: int):
        self.last_byte_bits_ = last_byte_bits
        self.value_ = val

import unittest

from Term import py_codec_impl as py_impl
import native_codec_impl as native_impl
from Term.atom import Atom
# from Term.pid import Pid
# from Term.reference import Reference
# from Term.fun import Fun
from Term.list import ImproperList


class TestETFEncode(unittest.TestCase):
    def test_encode_atom_py(self):
        self._encode_atom(py_impl)
        self._encode_atom_utf8(py_impl)

    def test_encode_atom_native(self):
        self._encode_atom(native_impl)
        self._encode_atom_utf8(native_impl)

    def _encode_atom(self, codec):
        """ Try an atom 'hello' encoded as Latin1 atom (16-bit length)
            or small atom (8bit length)
        """
        # Create and encode 'hello...hello' 52 times (260 bytes)
        # Expect UTF8 back because encoder only does UTF8 atoms
        repeat1 = 52
        example1 = bytes([131, py_impl.TAG_ATOM_UTF8_EXT, 1, 4]) \
                   + (b'hello' * repeat1)
        b1 = codec.term_to_binary(Atom("hello" * repeat1))
        self.assertEqual(b1, example1)

        # Create and encode 'hello...hello' 5 times (25 bytes)
        repeat2 = 5
        example2 = bytes([131, py_impl.TAG_SMALL_ATOM_UTF8_EXT, 25]) \
                   + (b'hello' * repeat2)
        b2 = codec.term_to_binary(Atom("hello" * repeat2))
        self.assertEqual(b2, example2)

    def _encode_atom_utf8(self, codec):
        # Create and encode 'hallå...hallå' 50 times (300 bytes)
        repeat1 = 50
        example1 = bytes([131, py_impl.TAG_ATOM_UTF8_EXT, 1, (300-256)]) \
                   + (bytes("hallå", "utf8") * repeat1)
        b1 = codec.term_to_binary(Atom("hallå" * repeat1))
        self.assertEqual(b1, example1)

        # Create and encode 'hallå...hallå' 5 times (30 bytes)
        repeat2 = 5
        example2 = bytes([131, py_impl.TAG_SMALL_ATOM_UTF8_EXT, 30]) \
                   + (bytes("hallå", "utf8") * repeat2)
        b2 = codec.term_to_binary(Atom("hallå" * repeat2))
        self.assertEqual(b2, example2)

    # ---------------------

    def test_encode_list_py(self):
        self._encode_list(py_impl)

    def test_encode_list_native(self):
        self._encode_list(native_impl)

    def _encode_list(self, codec):
        """ Encode list of something, an improper list and an empty list. """
        example1 = bytes([131, py_impl.TAG_LIST_EXT,
                          0, 0, 0, 2,  # length
                          py_impl.TAG_SMALL_INT, 1,
                          py_impl.TAG_ATOM_EXT, 0, 2, 111, 107,
                          py_impl.TAG_NIL_EXT])
        b1 = codec.term_to_binary([1, Atom("ok")])
        self.assertEqual(b1, example1)

        example2 = bytes([131, py_impl.TAG_LIST_EXT,
                          0, 0, 0, 1,  # length
                          py_impl.TAG_SMALL_INT, 1,
                          py_impl.TAG_ATOM_EXT, 0, 2, 111, 107])
        b2 = codec.term_to_binary(ImproperList([1], Atom("ok")))
        self.assertEqual(b2, example2)


if __name__ == '__main__':
    unittest.main()

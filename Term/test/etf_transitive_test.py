import unittest

from Term import py_codec_impl as py_impl
import native_codec_impl as native_impl
from Term.atom import Atom
from Term.bitstring import BitString


class TestETFEncode(unittest.TestCase):
    def test_encode_atom_py(self):
        self._encode_atom(py_impl)

    def test_encode_atom_native(self):
        self._encode_atom(native_impl)

    def _encode_atom(self, codec):
        """ Try an atom 'hello' """
        data1 = codec.term_to_binary(Atom('hello'))
        # expected1 = bytes([131, ord('d'), 0, 5, 104, 101, 108, 108, 111])
        expected1 = bytes([131, ord('v'), 0, 5, 104, 101, 108, 108, 111])
        self.assertEqual(data1, expected1)

        data2 = codec.term_to_binary(Atom('hello'))
        expected2 = bytes([131, ord('v'), 0, 5, 104, 101, 108, 108, 111])
        self.assertEqual(data2, expected2)

    # ----------------
    def test_encode_map_py(self):
        self._encode_map(py_impl)

    def test_encode_map_native(self):
        self._encode_map(native_impl)

    def _encode_map(self, codec):
        """ Try encode a map #{1 => 2} """
        data = codec.term_to_binary({1: 2})
        expected = bytes([131, 116, 0, 0, 0, 1, 97, 1, 97, 2])
        self.assertEqual(data, expected)

    # ----------------
    def test_encode_decode_pid_py(self):
        self._encode_decode_pid(py_impl)

    def test_encode_decode_pid_native(self):
        self._encode_decode_pid(native_impl)

    def _encode_decode_pid(self, codec):
        data1 = bytes([131, 103, ord('v'), 0, 13, 101, 114, 108, 64, 49, 50, 55,
                       46, 48, 46, 48, 46, 49, 0, 0, 0, 64, 0, 0, 0, 0, 1])
        (val1, tail) = codec.binary_to_term(data1, None)
        data2 = codec.term_to_binary(val1)
        self.assertEqual(data1, data2)

    # ----------------
    def test_encode_decode_ref_py(self):
        self._encode_decode_ref(py_impl)

    def test_encode_decode_ref_native(self):
        self._encode_decode_ref(native_impl)

    def _encode_decode_ref(self, codec):
        data1 = bytes([131, 114, 0, 3, ord('v'), 0, 13, 101, 114, 108, 64, 49, 50,
                       55, 46, 48, 46, 48, 46, 49, 1, 0, 0, 1, 58, 0, 0, 0, 2,
                       0, 0, 0, 0])
        (val1, tail) = codec.binary_to_term(data1, None)
        data2 = codec.term_to_binary(val1)
        self.assertEqual(data1, data2)

    # ----------------
    def test_float_py(self):
        self._float(py_impl)

    def test_float_native(self):
        self._float(native_impl)

    def _float(self, codec):
        """ Encode and decode immediately and compare results """
        val1 = 1.234567901
        data1 = codec.term_to_binary(val1)
        (val2, tail) = codec.binary_to_term(data1, None)
        self.assertEqual(val1, val2)
        self.assertEqual(tail, b'')

    # ----------------
    def test_binary_py(self):
        self._binary(py_impl)

    def test_binary_native(self):
        self._binary(native_impl)

    def _binary(self, codec):
        """ Encode and decode binary immediately and compare results """
        data1 = bytes([131, 109, 0, 0, 0, 1, 34])
        (val1, tail) = codec.binary_to_term(data1, None)
        data2 = codec.term_to_binary(val1)
        self.assertEqual(data1, data2)
        self.assertEqual(tail, b'')

    # ----------------
    def test_binary_bits_py(self):
        self._binary_bits(py_impl)

    def test_binary_bits_native(self):
        self._binary_bits(native_impl)

    def _binary_bits(self, codec):
        """ Encode and decode binary bits immediately and compare results.
            Erlang value is: <<3:2>>
        """
        data1 = bytes([131, 77, 0, 0, 0, 1, 2, 192])
        ((val1, lbbits1), tail) = codec.binary_to_term(data1, None)
        data2 = codec.term_to_binary(BitString(val1, lbbits1))
        self.assertEqual(data1, data2)
        self.assertEqual(tail, b'')


if __name__ == '__main__':
    unittest.main()

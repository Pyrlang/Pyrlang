import unittest

from Term import py_codec_impl as py_impl
import native_codec_impl as native_impl
from Term.atom import Atom
from Term.pid import Pid
from Term.reference import Reference
from Term.fun import Fun
from Term.list import list_to_unicode_str


class TestETFDecode(unittest.TestCase):
    def test_decode_atom_py(self):
        self._decode_atom(py_impl)

    def test_decode_atom_native(self):
        self._decode_atom(native_impl)

    def _decode_atom(self, codec):
        """ Try an atom 'hello' """
        b1 = bytes([131, 100, 0, 5, 104, 101, 108, 108, 111])
        (t1, tail1) = codec.binary_to_term(b1, None)
        self.assertTrue(isinstance(t1, Atom))
        self.assertEqual(t1.text_, "hello")
        self.assertEqual(tail1, b'')

    # ----------------

    def test_decode_atom_as_string_py(self):
        self._decode_atom_as_string(py_impl)

    def test_decode_atom_as_string_native(self):
        self._decode_atom_as_string(native_impl)

    def _decode_atom_as_string(self, codec):
        """ Try an atom 'hello' to a Python string """
        b1 = bytes([131, 100, 0, 5, 104, 101, 108, 108, 111])
        (t2, tail2) = codec.binary_to_term(b1, {"atom": "string"})
        self.assertTrue(isinstance(t2, str))
        self.assertEqual(t2, "hello")
        self.assertEqual(tail2, b'')

        (t3, tail3) = codec.binary_to_term(b1, {"atom": "bytes"})
        self.assertTrue(isinstance(t2, bytes))
        self.assertEqual(t3, b'hello')
        self.assertEqual(tail3, b'')

    # ----------------

    def test_decode_str_py(self):
        self._decode_str(py_impl)

    def test_decode_str_native(self):
        self._decode_str(native_impl)

    def _decode_str(self, codec):
        """ Try a simple ASCII string """
        b1 = bytes([131, 107, 0, 5, 104, 101, 108, 108, 111])
        (t1, tail) = codec.binary_to_term(b1, None)
        self.assertTrue(isinstance(t1, bytes))
        self.assertEqual(t1, b"hello")
        self.assertEqual(tail, b'')

        # Try a string with emoji, a list of unicode integers
        b1 = bytes([131, 108, 0, 0, 0, 3, 98, 0, 0, 38, 34, 97, 32, 98, 0, 0,
                    38, 35, 106])
        (t1, tail) = codec.binary_to_term(b1, None)
        self.assertTrue(isinstance(t1, list))
        self.assertEqual(tail, b'')
        self.assertEqual(list_to_unicode_str(t1), u"☢ ☣")

    # ----------------

    def test_decode_pid_py(self):
        self._decode_pid(py_impl)

    def test_decode_pid_native(self):
        self._decode_pid(native_impl)

    def _decode_pid(self, codec):
        """ Try a pid """
        data = bytes([131, 103, 100, 0, 13, 101, 114, 108, 64, 49, 50, 55, 46,
                      48, 46, 48, 46, 49, 0, 0, 0, 64, 0, 0, 0, 0, 1])
        (val, tail) = codec.binary_to_term(data, None)
        self.assertTrue(isinstance(val, Pid))
        self.assertEqual(tail, b'')

    # ----------------

    def test_decode_ref_py(self):
        self._decode_ref(py_impl)

    def test_decode_ref_native(self):
        self._decode_ref(native_impl)

    def _decode_ref(self, codec):
        """ Try a reference """
        b1 = bytes([131, 114, 0, 3, 100, 0, 13, 101, 114, 108, 64, 49, 50,
                    55, 46, 48, 46, 48, 46, 49, 1, 0, 0, 1, 58, 0, 0, 0, 2,
                    0, 0, 0, 0])
        (t1, tail) = codec.binary_to_term(b1, None)
        self.assertTrue(isinstance(t1, Reference))
        self.assertEqual(tail, b'')

    # ----------------

    def test_decode_map_py(self):
        self._decode_map(py_impl)

    def test_decode_map_native(self):
        self._decode_map(native_impl)

    def _decode_map(self, codec):
        """ Try a map #{1 => 2} """
        data = bytes([131, 116, 0, 0, 0, 1, 97, 1, 97, 2])
        (val, tail) = codec.binary_to_term(data, None)
        self.assertTrue(isinstance(val, dict))
        self.assertEqual(val, {1: 2})
        self.assertEqual(tail, b'')

    # ----------------

    def test_float_py(self):
        self._float(py_impl)

    def test_float_native(self):
        self._float(native_impl)

    def _float(self, codec):
        """ Try decode a prepared double Pi """
        data = bytes([131, 70, 64, 9, 33, 251, 84, 68, 45, 17])
        (val, tail) = codec.binary_to_term(data, None)
        self.assertEqual(val, 3.14159265358979)
        self.assertEqual(tail, b'')

    # ----------------

    def test_decode_binary_py(self):
        self._decode_binary(py_impl)

    def test_decode_binary_native(self):
        self._decode_binary(native_impl)

    def _decode_binary(self, codec):
        """ Decode binary to term.Binary and to Python bytes and compare.
            Binary is <<34>>.
        """
        data1 = bytes([131, 109, 0, 0, 0, 1, 34])
        (val1, tail1) = codec.binary_to_term(data1, None)
        self.assertEqual(val1, b'"')
        self.assertEqual(tail1, b'')

    # ----------------

    def test_decode_fun_py(self):
        self._decode_fun(py_impl)

    def test_decode_fun_native(self):
        self._decode_fun(native_impl)

    def _decode_fun(self, codec):
        data = bytes([131, 112, 0, 0, 0, 72, 0, 37, 73, 174, 126, 251, 115,
                      143, 183, 98, 224, 72, 249, 253, 111, 254, 159, 0, 0,
                      0, 0, 0, 0, 0, 1, 100, 0, 5, 116, 101, 115, 116, 49,
                      97, 0, 98, 1, 42, 77, 115, 103, 100, 0, 13, 110, 111,
                      110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116,
                      0, 0, 0, 58, 0, 0, 0, 0, 0, 97, 123])
        (val, tail) = codec.binary_to_term(data, None)
        self.assertTrue(isinstance(val, Fun))
        self.assertEqual(tail, b'')


if __name__ == '__main__':
    unittest.main()

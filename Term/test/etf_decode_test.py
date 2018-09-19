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
        self._decode_atom_utf8(py_impl)

    def test_decode_atom_native(self):
        self._decode_atom(native_impl)
        self._decode_atom_utf8(native_impl)

    def _decode_atom(self, codec):
        """ Try an atom 'hello' encoded as Latin1 atom (16-bit length)
            or small atom (8bit length)
        """
        b1 = bytes([131, py_impl.TAG_ATOM_EXT,
                    0, 5,
                    104, 101, 108, 108, 111])
        (t1, tail1) = codec.binary_to_term(b1, None)
        self.assertTrue(isinstance(t1, Atom), "Result must be Atom object")
        self.assertEqual(t1.text_, "hello")
        self.assertEqual(tail1, b'')

        b2 = bytes([131, py_impl.TAG_SMALL_ATOM_EXT,
                    5,
                    104, 101, 108, 108, 111])
        (t2, tail2) = codec.binary_to_term(b2, None)
        self.assertTrue(isinstance(t2, Atom), "Result must be Atom object")
        self.assertEqual(t2.text_, "hello")
        self.assertEqual(tail2, b'')

    def _decode_atom_utf8(self, codec):
        b1 = bytes([131, py_impl.TAG_ATOM_UTF8_EXT,
                    0, 6,
                    108, 195, 164, 103, 101, 116])
        (t1, tail1) = codec.binary_to_term(b1, None)
        self.assertTrue(isinstance(t1, Atom), "Result must be Atom object")
        self.assertTrue(isinstance(t1.text_, str), "Result .text_ field must be str")
        self.assertEqual(t1.text_, u"läget")
        self.assertEqual(tail1, b'')

    # ----------------

    def test_decode_atom_as_string_py(self):
        self._decode_atom_as_string(py_impl)

    def test_decode_atom_as_string_native(self):
        self._decode_atom_as_string(native_impl)

    def _decode_atom_as_string(self, codec):
        """ Try an atom 'hello' to a Python string """
        b1 = bytes([131, py_impl.TAG_ATOM_EXT,
                    0, 5,
                    104, 101, 108, 108, 111])
        (t2, tail2) = codec.binary_to_term(b1, {"atom": "str"})
        self.assertTrue(isinstance(t2, str),
                        "Expected str, have: " + t2.__class__.__name__)
        self.assertEqual(t2, "hello")
        self.assertEqual(tail2, b'')

        (t3, tail3) = codec.binary_to_term(b1, {"atom": "bytes"})
        self.assertTrue(isinstance(t3, bytes),
                        "Expected bytes, have: " + t3.__class__.__name__)
        self.assertEqual(t3, b'hello')
        self.assertEqual(tail3, b'')

    # ----------------

    def test_decode_str_py(self):
        self._decode_str_ascii(py_impl)
        self._decode_str_unicode(py_impl)

    def test_decode_str_native(self):
        self._decode_str_ascii(native_impl)
        self._decode_str_unicode(native_impl)

    def _decode_str_ascii(self, codec):
        """ A string with bytes, encoded as optimized byte array. """
        b1 = bytes([131, py_impl.TAG_STRING_EXT,
                    0, 5,
                    104, 101, 108, 108, 111])
        (t1, tail1) = codec.binary_to_term(b1, None)
        self.assertTrue(isinstance(t1, str), "Result must be str")
        self.assertEqual(t1, "hello")
        self.assertEqual(tail1, b'')

        (t2, tail2) = codec.binary_to_term(b1, {"byte_string": "bytes"})
        self.assertTrue(isinstance(t2, bytes),
                        "Result must be bytes, got " + t2.__class__.__name__)
        self.assertEqual(t2, b"hello")
        self.assertEqual(tail2, b'')

    def _decode_str_unicode(self, codec):
        """ A string with emoji, encoded as a list of unicode integers. """
        b1 = bytes([131, py_impl.TAG_LIST_EXT,
                    0, 0, 0, 3,  # length
                    py_impl.TAG_INT, 0, 0, 38, 34,  # 32-bit radiation hazard
                    py_impl.TAG_SMALL_INT, 32,      # 8-bit space (32)
                    py_impl.TAG_INT, 0, 0, 38, 35,  # 32-bit bio-hazard
                    py_impl.TAG_NIL_EXT  # list tail: NIL
                    ])
        (t1, tail) = codec.binary_to_term(b1, None)
        self.assertTrue(isinstance(t1, list), "Result must be a list")
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

    def test_decode_tuple_py(self):
        self._decode_tuple(py_impl)

    def test_decode_tuple_native(self):
        self._decode_tuple(native_impl)

    def _decode_tuple(self, codec):
        """ Try decode some tuple values """
        data1 = bytes([131, py_impl.TAG_SMALL_TUPLE_EXT,
                       2,
                       py_impl.TAG_SMALL_INT, 1,
                       py_impl.TAG_ATOM_EXT, 0, 2, 111, 107])
        (val1, tail1) = codec.binary_to_term(data1, None)
        self.assertEqual((1, Atom("ok")), val1)
        self.assertEqual(tail1, b'')

        data2 = bytes([131, py_impl.TAG_LARGE_TUPLE_EXT,
                       0, 0, 0, 2,
                       py_impl.TAG_SMALL_INT, 1,
                       py_impl.TAG_ATOM_EXT, 0, 2, 111, 107])
        (val2, tail2) = codec.binary_to_term(data2, None)
        self.assertEqual((1, Atom("ok")), val2)
        self.assertEqual(tail2, b'')

        # Empty tuple
        data3 = bytes([131, py_impl.TAG_SMALL_TUPLE_EXT, 0])
        (val3, tail3) = codec.binary_to_term(data3, None)
        self.assertEqual((), val3)
        self.assertEqual(tail3, b'')


# ----------------

    def test_decode_list_py(self):
        self._decode_list(py_impl)

    def test_decode_list_native(self):
        self._decode_list(native_impl)

    def _decode_list(self, codec):
        """ Try decode some list values """
        data1 = bytes([131, py_impl.TAG_NIL_EXT])
        (val1, tail1) = codec.binary_to_term(data1, None)
        self.assertEqual([], val1)
        self.assertEqual(tail1, b'')

        # Test data is [1, ok]
        data2 = bytes([131, py_impl.TAG_LIST_EXT,
                       0, 0, 0, 2,
                       py_impl.TAG_SMALL_INT, 1,
                       py_impl.TAG_ATOM_EXT, 0, 2, 111, 107,
                       py_impl.TAG_NIL_EXT])
        (val2, tail2) = codec.binary_to_term(data2, None)
        self.assertTrue(isinstance(val2, list),
                        "Expected list, got: %s (%s)"
                        % (val2.__class__.__name__, val2))
        self.assertEqual(val2, [1, Atom("ok")])
        self.assertEqual(tail2, b'')

    # ----------------

    def test_decode_map_py(self):
        self._decode_map(py_impl)

    def test_decode_map_native(self):
        self._decode_map(native_impl)

    def _decode_map(self, codec):
        """ Try a map #{1 => 2, ok => error} """
        data = bytes([131,
                      py_impl.TAG_MAP_EXT, 0, 0, 0, 2,
                      py_impl.TAG_SMALL_INT, 1,
                      py_impl.TAG_SMALL_INT, 2,
                      py_impl.TAG_ATOM_EXT, 0, 2, 111, 107,
                      py_impl.TAG_ATOM_EXT, 0, 5, 101, 114, 114, 111, 114])
        (val, tail) = codec.binary_to_term(data, None)
        self.assertTrue(isinstance(val, dict))
        self.assertEqual(val, {1: 2, Atom("ok"): Atom("error")})
        self.assertEqual(tail, b'')

    # ----------------

    def test_float_py(self):
        self._float(py_impl)

    def test_float_native(self):
        self._float(native_impl)

    def _float(self, codec):
        """ Try decode a prepared double Pi """
        data = bytes([py_impl.ETF_VERSION_TAG,
                      py_impl.TAG_NEW_FLOAT_EXT,  # a 8-byte IEEE double
                      64, 9, 33, 251, 84, 68, 45, 17])
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

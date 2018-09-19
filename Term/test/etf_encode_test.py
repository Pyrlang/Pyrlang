import unittest

from Term import py_codec_impl as py_impl
import native_codec_impl as native_impl
from Term.atom import Atom
from Term.pid import Pid
from Term.reference import Reference
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
        example1 = bytes([py_impl.ETF_VERSION_TAG,
                          py_impl.TAG_ATOM_UTF8_EXT, 1, 4]) \
                   + (b'hello' * repeat1)
        b1 = codec.term_to_binary(Atom("hello" * repeat1))
        self.assertEqual(b1, example1)

        # Create and encode 'hello...hello' 5 times (25 bytes)
        repeat2 = 5
        example2 = bytes([py_impl.ETF_VERSION_TAG,
                          py_impl.TAG_SMALL_ATOM_UTF8_EXT, 25]) \
                   + (b'hello' * repeat2)
        b2 = codec.term_to_binary(Atom("hello" * repeat2))
        self.assertEqual(b2, example2)

    def _encode_atom_utf8(self, codec):
        # Create and encode 'hallå...hallå' 50 times (300 bytes)
        repeat1 = 50
        example1 = bytes([py_impl.ETF_VERSION_TAG,
                          py_impl.TAG_ATOM_UTF8_EXT, 1, (300-256)]) \
                   + (bytes("hallå", "utf8") * repeat1)
        b1 = codec.term_to_binary(Atom("hallå" * repeat1))
        self.assertEqual(b1, example1)

        # Create and encode 'hallå...hallå' 5 times (30 bytes)
        repeat2 = 5
        example2 = bytes([py_impl.ETF_VERSION_TAG,
                          py_impl.TAG_SMALL_ATOM_UTF8_EXT, 30]) \
                   + (bytes("hallå", "utf8") * repeat2)
        b2 = codec.term_to_binary(Atom("hallå" * repeat2))
        self.assertEqual(b2, example2)

    # ---------------------

    def test_encode_str_py(self):
        self._encode_str(py_impl)
        self._encode_str_unicode(py_impl)

    def test_encode_str_native(self):
        self._encode_str(native_impl)
        self._encode_str_unicode(native_impl)

    def _encode_str(self, codec):
        # A 8-bit string max 65535 characters, optimized as byte array
        byte_example = bytes([py_impl.ETF_VERSION_TAG,
                              py_impl.TAG_STRING_EXT, 0, 5]) \
                       + bytes("hello", "latin-1")

        b1 = codec.term_to_binary("hello")
        self.assertEqual(b1, byte_example)

    def _encode_str_unicode(self, codec):
        # Unicode value for <A with RING ABOVE> is still within byte range
        # so this will produce a list of small ints
        unicode_example1 = bytes([py_impl.ETF_VERSION_TAG,
                                  py_impl.TAG_STRING_EXT, 0, 6]) \
                           + "hallå".encode("utf8")

        b1 = codec.term_to_binary("hallå")  # unicode but codepoints <= 255
        self.assertEqual(b1, unicode_example1)

        unicode_example2 = bytes([py_impl.ETF_VERSION_TAG,
                                  py_impl.TAG_LIST_EXT, 0, 0, 0, 2,
                                  py_impl.TAG_INT, 0, 0, 3, 148,
                                  py_impl.TAG_INT, 0, 0, 3, 169,
                                  py_impl.TAG_NIL_EXT])

        b2 = codec.term_to_binary("ΔΩ")  # unicode with large codepoints
        self.assertEqual(b2, unicode_example2)

    # ---------------------

    def test_encode_list_py(self):
        self._encode_list(py_impl)

    def test_encode_list_native(self):
        self._encode_list(native_impl)

    def _encode_list(self, codec):
        """ Encode list of something, an improper list and an empty list. """
        example1 = bytes([py_impl.ETF_VERSION_TAG,
                          py_impl.TAG_LIST_EXT,
                          0, 0, 0, 2,  # length
                          py_impl.TAG_SMALL_INT, 1,
                          py_impl.TAG_SMALL_ATOM_UTF8_EXT, 2, 111, 107,
                          py_impl.TAG_NIL_EXT])
        b1 = codec.term_to_binary([1, Atom("ok")])
        self.assertEqual(b1, example1)

        example2 = bytes([py_impl.ETF_VERSION_TAG, py_impl.TAG_LIST_EXT,
                          0, 0, 0, 1,  # length
                          py_impl.TAG_SMALL_INT, 1,
                          py_impl.TAG_SMALL_ATOM_UTF8_EXT, 2, 111, 107])
        b2 = codec.term_to_binary(ImproperList([1], Atom("ok")))
        self.assertEqual(b2, example2)

    # ----------------

    def test_encode_map_py(self):
        self._encode_map(py_impl)

    def test_encode_map_native(self):
        self._encode_map(native_impl)

    def _encode_map(self, codec):
        """ Try a map #{1 => 2, ok => error} """
        sample = bytes([py_impl.ETF_VERSION_TAG,
                        py_impl.TAG_MAP_EXT, 0, 0, 0, 2,
                        py_impl.TAG_SMALL_INT, 1,
                        py_impl.TAG_SMALL_INT, 2,
                        py_impl.TAG_SMALL_ATOM_UTF8_EXT, 2, 111, 107,
                        py_impl.TAG_SMALL_ATOM_UTF8_EXT, 5, 101, 114, 114, 111, 114])
        val = {1: 2, Atom("ok"): Atom("error")}
        bin1 = codec.term_to_binary(val)
        self.assertEqual(bin1, sample)

    # ----------------

    def test_encode_pid_py(self):
        self._encode_pid(py_impl)

    def test_encode_pid_native(self):
        self._encode_pid(native_impl)

    def _encode_pid(self, codec):
        sample1 = bytes([py_impl.ETF_VERSION_TAG,
                         py_impl.TAG_PID_EXT,
                         py_impl.TAG_SMALL_ATOM_UTF8_EXT, 13]) \
                  + bytes("nonode@nohost", "latin-1") \
                  + bytes([0, 0, 0, 1,
                           0, 0, 0, 2,
                           3])
        val = Pid("nonode@nohost", 1, 2, 3)
        bin1 = codec.term_to_binary(val)
        self.assertEqual(bin1, sample1)

    # ----------------

    def test_encode_ref_py(self):
        self._encode_ref(py_impl)

    def test_encode_ref_native(self):
        self._encode_ref(native_impl)

    def _encode_ref(self, codec):
        creation = 1
        sample1 = bytes([py_impl.ETF_VERSION_TAG,
                         py_impl.TAG_NEW_REF_EXT,
                         0, 3,  # length
                         py_impl.TAG_SMALL_ATOM_UTF8_EXT, 13,
                         110, 111, 110, 111, 100, 101, 64, 110, 111, 104, 111, 115, 116,
                         creation]) \
                  + bytes("fgsfdsfdsfgs", "latin-1")

        val = Reference("nonode@nohost", creation, b'fgsfdsfdsfgs')
        bin1 = codec.term_to_binary(val)
        self.assertEqual(bin1, sample1)

    # ----------------

    def test_encode_float_py(self):
        self._encode_float(py_impl)

    def test_encode_float_native(self):
        self._encode_float(native_impl)

    def _encode_float(self, codec):
        example1 = bytes([py_impl.ETF_VERSION_TAG,
                         py_impl.TAG_NEW_FLOAT_EXT,  # a 8-byte IEEE double
                         64, 9, 33, 251, 84, 68, 45, 17])
        b1 = codec.term_to_binary(3.14159265358979)
        self.assertEqual(b1, example1)


if __name__ == '__main__':
    unittest.main()

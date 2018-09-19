import unittest
import native_codec_impl as nativecodec
import Term.py_codec_impl as pycodec
from Term.atom import Atom


class TestNativeCodecDriver(unittest.TestCase):
    def test_simple_b2t_errors(self):
        with self.assertRaises(BaseException):
            nativecodec.binary_to_term(b'xxx', None)
            # Empty term creates empty input error
            nativecodec.binary_to_term(b'', None)

    def test_b2t_read_errors(self):
        with self.assertRaises(BaseException):
            # Compressed term (131, 80) with incomplete length field
            # should create read error
            nativecodec.binary_to_term(b'\x83\x50\x00', None)

    def test_b2t_library_equality_atoms(self):
        a = pycodec.term_to_binary(Atom("hello"))
        (b, _) = nativecodec.binary_to_term(a, None)
        self.assertEqual(Atom("hello"), b)


if __name__ == '__main__':
    unittest.main()

import unittest
import term_codec as rustcodec
import Term.codec as pycodec
from Term.atom import Atom


class TestNativeCodecDriver(unittest.TestCase):
    def test_simple_b2t_errors(self):
        with self.assertRaises(ValueError):
            rustcodec.binary_to_term(b'xxx')
            # Empty term creates empty input error
            rustcodec.binary_to_term(b'')

    def test_b2t_read_errors(self):
        with self.assertRaises(ValueError):
            # Compressed term (131, 80) with incomplete length field
            # should create read error
            rustcodec.binary_to_term(b'\x83\x50\x00')

    def test_b2t_library_equality_atoms(self):
        a = pycodec.term_to_binary(Atom('hello'))
        b = rustcodec.binary_to_term(a)
        self.assertEqual(a, b)


if __name__ == '__main__':
    unittest.main()

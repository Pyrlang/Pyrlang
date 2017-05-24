import unittest

import sys

sys.path.insert(0, '.')

from Pyrlang.Dist import etf
from Pyrlang import term


class TestETFDecode(unittest.TestCase):
    def test_decode_atom(self):
        """ Try an atom 'hello' """
        b1 = bytes([131, 100, 0, 5, 104, 101, 108, 108, 111])
        (t1, tail) = etf.binary_to_term(b1)
        self.assertTrue(isinstance(t1, term.Atom))
        self.assertEqual(t1.text_, "hello")
        self.assertEqual(tail, b'')

    def test_decode_str(self):
        """ Try a simple ASCII string """
        b1 = bytes([131, 107, 0, 5, 104, 101, 108, 108, 111])
        (t1, tail) = etf.binary_to_term(b1)
        self.assertTrue(isinstance(t1, str))
        self.assertEqual(t1, "hello")
        self.assertEqual(tail, b'')

    def test_decode_unicode_string(self):
        """ Try a string with emoji, a list of unicode integers """
        b1 = bytes([131, 108, 0, 0, 0, 3, 98, 0, 0, 38, 34, 97, 32, 98, 0, 0,
                    38, 35, 106])
        (t1, tail) = etf.binary_to_term(b1)
        self.assertTrue(isinstance(t1, term.List))
        self.assertEqual(tail, b'')
        self.assertEqual(t1.as_unicode(), u"☢ ☣")

    def test_decode_pid(self):
        """ Try a pid """
        data = bytes([131, 103, 100, 0, 13, 101, 114, 108, 64, 49, 50, 55, 46,
                      48, 46, 48, 46, 49, 0, 0, 0, 64, 0, 0, 0, 0, 1])
        (val, tail) = etf.binary_to_term(data)
        self.assertTrue(isinstance(val, term.Pid))
        self.assertEqual(tail, b'')

    def test_decode_ref(self):
        """ Try a reference """
        b1 = bytes([131, 114, 0, 3, 100, 0, 13, 101, 114, 108, 64, 49, 50,
                    55, 46, 48, 46, 48, 46, 49, 1, 0, 0, 1, 58, 0, 0, 0, 2,
                    0, 0, 0, 0])
        (t1, tail) = etf.binary_to_term(b1)
        self.assertTrue(isinstance(t1, term.Reference))
        self.assertEqual(tail, b'')

    def test_decode_map(self):
        """ Try a map #{1 => 2} """
        data = bytes([131, 116, 0, 0, 0, 1, 97, 1, 97, 2])
        (val, tail) = etf.binary_to_term(data)
        self.assertTrue(isinstance(val, dict))
        self.assertEqual(val, {1: 2})
        self.assertEqual(tail, b'')

    def test_float(self):
        """ Try decode a prepared double Pi """
        data = bytes([131, 70, 64, 9, 33, 251, 84, 68, 45, 17])
        (val, tail) = etf.binary_to_term(data)
        self.assertEqual(val, 3.14159265358979)
        self.assertEqual(tail, b'')

if __name__ == '__main__':
    unittest.main()

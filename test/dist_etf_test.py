import unittest

import sys

sys.path.insert(0, '.')

from Pyrlang.Dist import etf
from Pyrlang import term


class TestETF(unittest.TestCase):
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
        b1 = bytes([131, 103, 100, 0, 13, 101, 114, 108, 64, 49, 50, 55, 46,
                    48, 46, 48, 46, 49, 0, 0, 0, 64, 0, 0, 0, 0, 1])
        (t1, tail) = etf.binary_to_term(b1)
        self.assertTrue(isinstance(t1, term.Pid))
        print(t1)
        self.assertEqual(tail, b'')

    def test_decode_ref(self):
        """ Try a reference """
        b1 = bytes([131, 114, 0, 3, 100, 0, 13, 101, 114, 108, 64, 49, 50,
                    55, 46, 48, 46, 48, 46, 49, 1, 0, 0, 1, 58, 0, 0, 0, 2,
                    0, 0, 0, 0])
        (t1, tail) = etf.binary_to_term(b1)
        self.assertTrue(isinstance(t1, term.Reference))
        print(t1)
        self.assertEqual(tail, b'')


if __name__ == '__main__':
    unittest.main()

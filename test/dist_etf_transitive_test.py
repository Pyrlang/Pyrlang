import unittest

import sys

sys.path.insert(0, '.')

from Pyrlang.Dist import etf
from Pyrlang import term


class TestETFEncode(unittest.TestCase):
    def test_encode_atom(self):
        """ Try an atom 'hello' """
        data = etf.term_to_binary(term.Atom('hello'))
        expected = bytes([131, 100, 0, 5, 104, 101, 108, 108, 111])
        self.assertEqual(data, expected)

    def test_encode_map(self):
        """ Try encode a map #{1 => 2} """
        data = etf.term_to_binary({1: 2})
        expected = bytes([131, 116, 0, 0, 0, 1, 97, 1, 97, 2])
        self.assertEqual(data, expected)

    def test_encode_decode_pid(self):
        data1 = bytes([131, 103, 100, 0, 13, 101, 114, 108, 64, 49, 50, 55, 46,
                      48, 46, 48, 46, 49, 0, 0, 0, 64, 0, 0, 0, 0, 1])
        (val1, tail) = etf.binary_to_term(data1)
        data2 = etf.term_to_binary(val1)
        self.assertEqual(data1, data2)

    def test_encode_decode_ref(self):
        data1 = bytes([131, 114, 0, 3, 100, 0, 13, 101, 114, 108, 64, 49, 50,
                       55, 46, 48, 46, 48, 46, 49, 1, 0, 0, 1, 58, 0, 0, 0, 2,
                       0, 0, 0, 0])
        (val1, tail) = etf.binary_to_term(data1)
        data2 = etf.term_to_binary(val1)
        self.assertEqual(data1, data2)

    def test_float(self):
        """ Encode and decode immediately and compare results """
        val1 = 1.234567901
        data1 = etf.term_to_binary(val1)
        (val2, tail) = etf.binary_to_term(data1)
        self.assertEqual(val1, val2)
        self.assertEqual(tail, b'')

    def test_binary(self):
        """ Encode and decode binary immediately and compare results """
        data1 = bytes([131, 109, 0, 0, 0, 1, 34])
        (val1, tail) = etf.binary_to_term(data1)
        data2 = etf.term_to_binary(val1)
        self.assertEqual(data1, data2)
        self.assertEqual(tail, b'')

    def test_binary_bits(self):
        """ Encode and decode binary bits immediately and compare results """
        data1 = bytes([131, 77, 0, 0, 0, 1, 2, 192])
        (val1, tail) = etf.binary_to_term(data1)
        data2 = etf.term_to_binary(val1)
        self.assertEqual(data1, data2)
        self.assertEqual(tail, b'')


if __name__ == '__main__':
    unittest.main()

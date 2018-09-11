import unittest
import term_codec as co


class TestNativeCodecDriver(unittest.TestCase):
    def test_simple_b2t_errors(self):
        with self.assertRaises(ValueError):
            co.binary_to_term(b'xxx')
            co.binary_to_term(b'')


if __name__ == '__main__':
    unittest.main()

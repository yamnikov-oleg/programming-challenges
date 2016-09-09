import unittest

from main import *

class ToDec(unittest.TestCase):
    def assertToDec(self, fib, dec):
        self.assertEqual(to_dec(fib), dec)

    def test_3(self):
        self.assertToDec('110', 3)

    def test_16(self):
        self.assertToDec('1001000', 16)

    def test_19(self):
        self.assertToDec('1010010', 19)

    def test_32(self):
        self.assertToDec('10101000', 32)

    def test_9024720(self):
        self.assertToDec('1010100101010100000010001000010010', 9024720)

class ToFibMin(unittest.TestCase):
    def assertToFibMin(self, dec, fib):
        self.assertEqual(to_fib_min(dec), fib)

    def test_3(self):
        self.assertToFibMin(3, '1000')

    def test_7(self):
        self.assertToFibMin(7, '10100')

    def test_11(self):
        self.assertToFibMin(11, '101000')

    def test_20(self):
        self.assertToFibMin(20, '1010100')

    def test_28(self):
        self.assertToFibMin(28, '10010100')

class ToFibMax(unittest.TestCase):
    def assertToFibMax(self, dec, fib):
        self.assertEqual(to_fib_max(dec), fib)

    def test_3(self):
        self.assertToFibMax(3, '101')

    def test_7(self):
        self.assertToFibMax(7, '1111')

    def test_11(self):
        self.assertToFibMax(11, '11101')

    def test_20(self):
        self.assertToFibMax(20, '111111')

    def test_28(self):
        self.assertToFibMax(28, '1101111')

if __name__ == '__main__':
    unittest.main()

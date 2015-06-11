#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
import unittest
from jimn.precision import *


class test_precision(unittest.TestCase):

    def test_is_almost(self):
        f1 = 3
        f2 = f1 + limit/10
        f3 = f1 + limit/2
        f4 = f1 + limit*0.9
        f5 = f1 - limit*0.9
        self.assertTrue(is_almost(f1, f2))
        self.assertTrue(is_almost(f1, f3))
        self.assertTrue(is_almost(f1, f4))
        self.assertTrue(is_almost(f1, f5))
        f6 = f1 + limit*1.0001
        f7 = f1 - limit*1.0001
        self.assertFalse(is_almost(f1, f6))
        self.assertFalse(is_almost(f1, f7))

    def test_keys(self):
        self.assertTrue(coordinate_key(0.0) == coordinate_key(-0.0))
        for c in (3, 103):
            self.assertTrue(coordinate_key(c) == coordinate_key(c-limit*0.49))
            self.assertTrue(coordinate_key(c) == coordinate_key(c-limit*0.49))
            self.assertFalse(coordinate_key(c) == coordinate_key(c+limit*0.51))
            self.assertFalse(coordinate_key(c) == coordinate_key(c-limit*0.51))
            self.assertTrue(displaced_coordinate_key(c) == coordinate_key(c+limit*0.51))

if __name__ == '__main__':
    unittest.main()

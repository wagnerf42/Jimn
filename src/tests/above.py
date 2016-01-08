#!/usr/bin/env python3
import unittest
from jimn.segment import Segment
from jimn.arc import arc
from jimn.point import Point


class test_above(unittest.TestCase):

    def test_above(self):
        s = Segment(
            [
                Point([-0.42275514588561586, -1.026519556967341]),
                Point([-0.49377030153582446, -1.1979593675717173])
            ]
        )

        a = arc(
            0.22,
            [
                Point([-0.42275514588561586, -1.026519556967341]),
                Point([-0.4227551484692215, -0.9734804217559714])
            ],
            Point([-0.6411509175897767, -1.0]),
            False
        )
        self.assertTrue(s.is_above(a))

    def test2(self):
        a1 = arc(
            0.3,
            [
                Point([-0.18858590833869351, -0.99999998083577]),
                Point([-0.341454458201092, -0.9865080679705822])
            ],
            Point([-0.2905178359359928, -1.2821522130372867]),
            False
        )
        a2 = arc(
            0.3,
            [
                Point([-0.3924497635332921, -0.99999998083577]),
                Point([-0.34145445648887895, -1.0134918939959543])
            ],
            Point([-0.2905178359359928, -0.717847748634252]),
            False
        )
        self.assertFalse(a1.is_above(a2))


if __name__ == '__main__':
    unittest.main()

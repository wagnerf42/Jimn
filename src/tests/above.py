#!/usr/bin/env python3
import unittest
from jimn.segment import segment
from jimn.arc import arc
from jimn.point import point


class test_above(unittest.TestCase):

    def test_above(self):
        s = segment(
            [
                point([-0.42275514588561586, -1.026519556967341]),
                point([-0.49377030153582446, -1.1979593675717173])
            ]
        )

        a = arc(
            0.22,
            [
                point([-0.42275514588561586, -1.026519556967341]),
                point([-0.4227551484692215, -0.9734804217559714])
            ],
            point([-0.6411509175897767, -1.0]),
            False
        )
        self.assertTrue(s.is_above(a))

if __name__ == '__main__':
    unittest.main()

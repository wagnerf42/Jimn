#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
import unittest
from jimn.segment import segment
from jimn.point import point


class test_segments(unittest.TestCase):

    def test_point_equality(self):
        p1 = point([1, 2])
        p2 = point([1, 2])
        self.assertEqual(p1, p2)

    def test_point_inequality(self):
        p1 = point([1, 2])
        p2 = point([1, 3])
        self.assertNotEqual(p1, p2)

    def test_segment_equality(self):
        s1 = segment([
            point([1, 2]),
            point([3, 4])
        ])
        s2 = segment([
            point([1, 2]),
            point([3, 4])
        ])
        self.assertEqual(s1, s2)

if __name__ == '__main__':
    unittest.main()

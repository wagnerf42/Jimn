#!/usr/bin/env python3
import unittest
from jimn.segment import Segment
from jimn.point import Point


class test_segments(unittest.TestCase):

    def test_point_equality(self):
        p1 = Point([1, 2])
        p2 = Point([1, 2])
        self.assertEqual(p1, p2)

    def test_point_inequality(self):
        p1 = Point([1, 2])
        p2 = Point([1, 3])
        self.assertNotEqual(p1, p2)

    def test_segment_equality(self):
        s1 = Segment([
            Point([1, 2]),
            Point([3, 4])
        ])
        s2 = Segment([
            Point([1, 2]),
            Point([3, 4])
        ])
        self.assertEqual(s1, s2)

    def test_intersection(self):
        s1 = Segment([Point([1.31, 1.31]), Point([1.31, -1.31])])
        s2 = Segment([Point([-1.31, 1.2000000000000002]),
                      Point([1.31, 1.2000000000000002])])
        intersections = s1.intersections_with(s2)
        self.assertEqual(len(intersections), 1)


if __name__ == '__main__':
    unittest.main()

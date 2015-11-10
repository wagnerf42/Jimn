#!/usr/bin/env python3
import unittest
from jimn.segment import segment
from jimn.point import point
from jimn.utils.coordinates_hash import rounder2d


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

    def test_intersection(self):
        s1 = segment([point([1.31, 1.31]), point([1.31, -1.31])])
        s2 = segment([point([-1.31, 1.2000000000000002]),
                      point([1.31, 1.2000000000000002])])
        intersections = s1.intersections_with(s2, rounder2d)
        self.assertEqual(len(intersections), 1)


if __name__ == '__main__':
    unittest.main()

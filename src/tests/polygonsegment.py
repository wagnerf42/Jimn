#!/usr/bin/env python3
import unittest
from jimn.polygonsegment import polygonsegment
from jimn.point import Point

horizontal_segment = polygonsegment([Point([0, 0]), Point([4, 0])], 3, 0)


class test_polygonsegment(unittest.TestCase):

    def test_above_included(self):
        s = (polygonsegment([Point([0, 3]), Point([4, 3])], 3, 0))
        self.assertLess(horizontal_segment, s)

    def test_above_on_interval(self):
        s = (polygonsegment([Point([-5, -1]), Point([4, 9])], 3, 0))
        self.assertLess(horizontal_segment, s)

    def test_same_starting_point(self):
        s = (polygonsegment([Point([0, 0]), Point([3, 0])], 2, 0))
        self.assertLess(horizontal_segment, s)

if __name__ == '__main__':
    unittest.main()

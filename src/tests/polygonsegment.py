#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
import unittest
from jimn.polygonsegment import polygonsegment
from jimn.point import point

horizontal_segment = polygonsegment([point([0, 0]), point([4, 0])], 3, 0)


class test_polygonsegment(unittest.TestCase):

    def test_above_included(self):
        s = (polygonsegment([point([0, 3]), point([4, 3])], 3, 0))
        self.assertLess(horizontal_segment, s)

    def test_above_on_interval(self):
        s = (polygonsegment([point([-5, -1]), point([4, 9])], 3, 0))
        self.assertLess(horizontal_segment, s)

    def test_same_starting_point(self):
        s = (polygonsegment([point([0, 0]), point([3, 0])], 2, 0))
        self.assertLess(horizontal_segment, s)

if __name__ == '__main__':
    unittest.main()

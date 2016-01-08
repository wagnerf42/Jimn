#!/usr/bin/env python3

from jimn.point import Point
from jimn.segment import Segment
from jimn.displayable import tycat
from jimn.displayable import tycat_set_svg_dimensions
from jimn.algorithms.segment_merger import merge_segments

tycat_set_svg_dimensions(800, 600)


s = [
    Segment([
        Point([1, 1]),
        Point([2, 2])
    ]),
    Segment([
        Point([0, 0]),
        Point([4, 4])
    ]),
    Segment([
        Point([4, 4]),
        Point([5, 5])
    ]),
    Segment([
        Point([5, 5]),
        Point([7, 7])
    ]),
    Segment([
        Point([6, 6]),
        Point([8, 8])
    ]),
]

tycat(*s)
merged = merge_segments(s)
tycat(merged)
print('answer is [0-1] [2-6] [7-8]')

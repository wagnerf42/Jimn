#!/usr/bin/env python3

from jimn.point import Point
from jimn.segment import segment
from jimn.displayable import tycat
from jimn.displayable import tycat_set_svg_dimensions
from jimn.algorithms.segment_merger import merge_segments

tycat_set_svg_dimensions(800, 600)


s = [
    segment([
        Point([1, 1]),
        Point([2, 2])
    ]),
    segment([
        Point([0, 0]),
        Point([4, 4])
    ]),
    segment([
        Point([4, 4]),
        Point([5, 5])
    ]),
    segment([
        Point([5, 5]),
        Point([7, 7])
    ]),
    segment([
        Point([6, 6]),
        Point([8, 8])
    ]),
]

tycat(*s)
merged = merge_segments(s)
tycat(merged)
print('answer is [0-1] [2-6] [7-8]')

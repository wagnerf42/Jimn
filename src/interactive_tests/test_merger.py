#!/usr/bin/env python3

from jimn.point import point
from jimn.segment import segment
from jimn.displayable import tycat
from jimn.displayable import tycat_set_svg_dimensions
from jimn.algorithms.segment_merger import merge_segments

tycat_set_svg_dimensions(800, 600)


s = [
    segment([
        point([1, 1]),
        point([2, 2])
    ]),
    segment([
        point([0, 0]),
        point([4, 4])
    ]),
    segment([
        point([4, 4]),
        point([5, 5])
    ]),
    segment([
        point([5, 5]),
        point([7, 7])
    ]),
    segment([
        point([6, 6]),
        point([8, 8])
    ]),
]

tycat(*s)
merged = merge_segments(s)
tycat(merged)
print('answer is [0-1] [2-6] [7-8]')

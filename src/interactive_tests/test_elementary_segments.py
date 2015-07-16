#!/usr/bin/python3

from jimn.point import point
from jimn.segment import segment
from jimn.segments_set import segments_set
from jimn.displayable import tycat
import random

segments = []
for i in range(10):
    s = segment([
        point([
            random.random(), random.random()
        ]),
        point([
            random.random(), random.random()
        ]),
    ])
    segments.append(s)

big = segments_set(segments)
tycat(*big.get_segments())
small = big.compute_elementary_segments(segments)
tycat(*small.get_segments())


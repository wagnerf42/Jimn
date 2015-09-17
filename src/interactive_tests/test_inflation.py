#!/usr/bin/env python3

from jimn.point import point
from jimn.segment import segment
from jimn.path_merger import inflate_segment
from jimn.displayable import tycat

s = segment([
    point([2, 2]),
    point([6, 5])
])

p = inflate_segment(s, 1.1)
tycat(s, p)

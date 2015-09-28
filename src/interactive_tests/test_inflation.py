#!/usr/bin/env python3

from jimn.point import point
from jimn.segment import segment
from jimn.arc import arc
from jimn.path_merger import inflate_segment, inflate_arc
from jimn.displayable import tycat

s = segment([
    point([2, 2]),
    point([6, 5])
])

a = arc(1, [point([2,3]), point([2,5])], point([2,4]))
p = inflate_arc(a, 1)

e = s.overlapping_area_exit_point(a, 1)
tycat(s, p, e)

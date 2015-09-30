#!/usr/bin/env python3

from jimn.point import point
from jimn.segment import segment
from jimn.arc import arc
from jimn.tree.path_tree.path_merger import inflate_arc, \
    overlapping_area_exit_point
from jimn.displayable import tycat

s = segment([
    point([2, 2]),
    point([8, 5])
])

a = arc(1, [point([2, 3]), point([2, 5])], point([2, 4]))
p = inflate_arc(a, 1)

e = overlapping_area_exit_point(s, a, 1, 1)
tycat(s, p, e.p)

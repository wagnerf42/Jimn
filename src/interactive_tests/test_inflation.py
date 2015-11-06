#!/usr/bin/env python3

from jimn.point import point
from jimn.segment import segment
from jimn.arc import arc
from jimn.tree.path_tree.path_merger import inflate_arc, \
    overlapping_area_exit_point
from jimn.displayable import tycat

s1 = segment(
    [point([1.4120979968363845, -0.55]), point([1.432806788453877, -0.5])]
)

s2 = segment(
    [point([1.46, -0.6000000000000001]), point([1.46, -0.65])]
)

e = overlapping_area_exit_point(s1, s2, 0.05, 1)
tycat(s1, s2, e.outer_point, e.inner_point)

s = segment([
    point([2, 2]),
    point([8, 5])
])

a = arc(1, [point([2, 3]), point([2, 5])], point([2, 4]))
p = inflate_arc(a, 1)

e = overlapping_area_exit_point(s, a, 1, 1)
tycat(s, p, e.outer_point, e.inner_point)

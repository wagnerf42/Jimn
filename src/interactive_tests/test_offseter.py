#!/usr/bin/env python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.displayable import tycat
from jimn.offseter import offset_holed_polygon

p = polygon([
    point([0, 0]),
    point([1, 1]),
    point([2, 0]),
    point([2, 4]),
    point([1, 5]),
    point([0, 4])
])

print("clockwise ?", p.is_oriented_clockwise())
tycat(p, *p.get_points())

offset_holed_polygon(0.3, p)
p.orient(False)
offset_holed_polygon(0.3, p)

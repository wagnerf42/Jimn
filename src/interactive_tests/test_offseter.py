#!/usr/bin/env python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.displayable import tycat
from jimn.offseter import offset_holed_polygon

p = polygon([
    point([0, 0]),
    point([1, -2]),
    point([2, -0.6]),
    point([3, -2]),
    point([4, 0]),
])

print("clockwise ?", p.is_oriented_clockwise())
tycat(p, *p.get_points())
tycat(p, *p.segments())

#ghost = offset_holed_polygon(0.5, p)
#tycat(p, *ghost)
p.orient(False)
ghost = offset_holed_polygon(0.5, p)
tycat(p, *ghost)

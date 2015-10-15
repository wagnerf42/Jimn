#!/usr/bin/env python3

from jimn.polygon import polygon
from jimn.displayable import tycat

p = polygon.square(0, 0, 3)
tycat(*p.get_points())
print("oriented clockwise ?", p.is_oriented_clockwise())

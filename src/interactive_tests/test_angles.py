#!/usr/bin/env python3

from jimn.point import point
from jimn.displayable import tycat
from math import cos, sin, pi


points = [point([cos((i*2*pi)/8),sin(i*(2*pi)/8)]) for i in range(8)]
tycat(*points)
origin = point([0, 0])
for p in points:
    print(origin.angle_with(p))

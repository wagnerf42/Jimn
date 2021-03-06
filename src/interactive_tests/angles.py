#!/usr/bin/env python3

from jimn.point import Point
from jimn.segment import Segment
from jimn.displayable import tycat
from math import cos, sin, pi


points = [Point([cos((i*2*pi)/8),sin(i*(2*pi)/8)]) for i in range(8)]
tycat(*points)
origin = Point([0, 0])
for p in points:
    print(origin.angle_with(p))

for p in points:
    s = Segment([origin, p])
    tycat(points, s)
    print("key angle is:", s.key_angle())
    print("********************")

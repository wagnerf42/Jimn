#!/usr/bin/env python3

from jimn.point import Point
from jimn.segment import segment
from jimn.polygon import polygon
from jimn.pocket import pocket
from jimn.envelope import envelope
from jimn.displayable import tycat

s = polygon.square(0, 0, 8)
s.orient(False)
path_in = pocket(list(s.segments()))

path_out = segment([
    Point([-3, 3]), Point([12, 5])
])

e1 = envelope(path_in, 1)
e2 = envelope(path_out, 1)

tycat(e1, e2)

points = e2.junction_points(e1)
tycat(e1, e2, points)

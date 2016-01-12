#!/usr/bin/env python3

from jimn.point import Point
from jimn.segment import Segment
from jimn.polygon import polygon
from jimn.pocket import pocket
from jimn.envelope import Envelope
from jimn.displayable import tycat

s = polygon.square(0, 0, 8)
s.orient(False)
path_in = pocket(list(s.segments()))

path_out = Segment([
    Point([-3, 3]), Point([12, 5])
])

e1 = Envelope(path_in, 1)
e2 = Envelope(path_out, 1)

tycat(e1, e2)

points = e2.junction_points(e1)
tycat(e1, e2, points)

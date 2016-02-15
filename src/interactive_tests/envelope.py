#!/usr/bin/env python3

from jimn.point import Point
from jimn.segment import Segment
from jimn.polygon import Polygon
from jimn.pocket import Pocket
from jimn.envelope import Envelope
from jimn.arc import Arc
from jimn.displayable import tycat
from jimn.displayable import tycat_start, tycat_end

print("testing intersection")

s = Polygon.square(0, 0, 8)
s = s.orient(False)
path_in = Pocket(list(s.segments()))

path_out = Segment([
    Point([-3, 3]), Point([12, 5])
])

e1 = Envelope(path_in, 1)
e2 = Envelope(path_out, 1)

tycat(e1, e2)

points = e2.junction_points(e1)
tycat(e1, e2, points)

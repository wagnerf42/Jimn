#!/usr/bin/env python3

from jimn.point import Point
from jimn.Segment import Segment
from jimn.arc import arc
from jimn.pocket import pocket
from jimn.displayable import tycat
from jimn.pocket.builder import build_pockets

print("test to follow all paths in a given pocket, building sub-pockets")
p = pocket([
    Segment([Point([0.0, -0.5]), Point([0.5, -0.5])]),
    Segment([Point([0.5, 1.0]), Point([0.0, 1.0])]),
    Segment([Point([1.0, 0.0]), Point([1.0, 0.5])]),
    Segment([Point([-0.5, 0.5]), Point([-0.5, 0.0])]),
    arc(1, [Point([-0.5, 0.5]), Point([-0.46824583655185426, 0.25])],
        Point([0.5, 0.5]), True),
    arc(1, [Point([-0.46824583655185426, 0.25]),
            Point([0.25, -0.46824583655185426])], Point([0.5, 0.5]), True),
    arc(1, [Point([0.25, -0.46824583655185426]),
            Point([0.5, -0.5])], Point([0.5, 0.5]), True),
    arc(1, [Point([0.5, 1.0]), Point([0.25, 0.9682458365518543])],
        Point([0.5, 0.0]), True),
    arc(1, [Point([0.25, 0.9682458365518543]),
            Point([-0.46824583655185426, 0.25])], Point([0.5, 0.0]), True),
    arc(1, [Point([-0.46824583655185426, 0.25]), Point([-0.5, 0.0])],
        Point([0.5, 0.0]), True),
    arc(1, [Point([1.0, 0.0]), Point([0.9682458365518543, 0.25])],
        Point([0.0, 0.0]), True),
    arc(1, [Point([0.9682458365518543, 0.25]),
            Point([0.25, 0.9682458365518543])], Point([0.0, 0.0]), True),
    arc(1, [Point([0.25, 0.9682458365518543]),
            Point([0.0, 1.0])], Point([0.0, 0.0]), True),
    arc(1, [Point([0.0, -0.5]), Point([0.25, -0.46824583655185426])],
        Point([0.0, 0.5]), True),
    arc(1, [Point([0.25, -0.46824583655185426]),
            Point([0.9682458365518543, 0.25])], Point([0.0, 0.5]), True),
    arc(1, [Point([0.9682458365518543, 0.25]), Point([1.0, 0.5])],
        Point([0.0, 0.5]), True),
])

tycat(p)

sub_pockets = build_pockets(p.get_content())
print("results")
tycat(*sub_pockets)


print("two pockets connected by one point")
print("should create two different pockets")
a = Point([0.0, 3.0])
b = Point([0.0, 0.0])
c = Point([3.0, 0.0])
d = Point([6.0, 0.0])
e = Point([6.0, 3.0])

pockets = build_pockets(
    [
        Segment([a, b]),
        Segment([a, c]),
        Segment([b, c]),
        Segment([c, d]),
        Segment([c, e]),
        Segment([d, e]),
    ]
)

tycat(*pockets)

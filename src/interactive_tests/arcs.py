#!/usr/bin/env python3

from jimn.point import Point
from jimn.segment import Segment
from jimn.arc import Arc
from jimn.displayable import tycat

labels = (
    "testing with double intersections",
    "testing with single intersection",
    "testing with no intersection (too far)",
    "testing with no intersection (same center)",
)

a1 = Arc(
    3, [Point([0, 0]), Point([3, 3])], Point([0, 3])
)
print("radius is", a1.radius, "angle is", a1.angle(), "length is", a1.length())
tycat(a1)

arcs = (
    Arc(
        3, [Point([1, 2]), Point([4, -1])], Point([4, 2])
    ),
    Arc(
        3, [Point([3, 3]), Point([6, 0])], Point([6, 3])
    ),
    Arc(
        3, [Point([4, 3]), Point([7, 0])], Point([7, 3])
    ),
    Arc(
        2, [Point([0, 1]), Point([2, 3])], Point([0, 3])
    )
)


print("*** trying intersection with arcs ***")
for i, a in enumerate(arcs):
    print(labels[i])
    intersection = a1.intersections_with_arc(a)
    tycat(a1, a, intersection)

segments = (
    Segment([Point([-1, 2]), Point([3, 1])]),
    Segment([Point([3.2, 3]), Point([0.2, 0])]),
    Segment([Point([5.2, 3]), Point([2.2, 0])]),
    Segment([Point([-1, 0]), Point([3, 0])]),
)
s_labels = (
    "single intersection",
    "double intersection",
    "no intersection",
    "tangent",
)
print("*** trying intersection with segments ***")
for i, s in enumerate(segments):
    print(s_labels[i])
    intersections = a1.intersections_with_segment(s)
    tycat(a1, s, intersections)

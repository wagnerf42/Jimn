#!/usr/bin/env python3

from jimn.point import point
from jimn.segment import segment
from jimn.arc import arc
from jimn.displayable import tycat

labels = (
    "testing with double intersections",
    "testing with single intersection",
    "testing with no intersection (too far)",
    "testing with no intersection (same center)",
    "testing with no intersection (but close enough)",
)

a1 = arc(
    3, [point([0, 0]), point([3, 3])]
)

arcs = (
    arc(
        3, [point([1, 2]), point([4, -1])]
    ),
    arc(
        3, [point([3, 3]), point([6, 0])]
    ),
    arc(
        3, [point([4, 3]), point([7, 0])]
    ),
    arc(
        2, [point([0, 1]), point([2, 3])]
    ),
    arc(
        3, [point([1, -0.5]), point([4, -3.5])]
    )
)


print("*** trying intersection with arcs ***")
for i, a in enumerate(arcs):
    print(labels[i])
    intersection = a1.intersections_with_arc(a)
    tycat(a1, a, intersection)

segments = (
    segment([point([-1, 2]), point([3, 1])]),
    segment([point([3.2, 3]), point([0.2, 0])]),
    segment([point([5.2, 3]), point([2.2, 0])]),
    segment([point([-1, 0]), point([3, 0])]),
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

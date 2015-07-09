#!/usr/bin/env python3

from jimn.point import point
from jimn.arc import arc
from jimn.displayable import tycat

a1 = arc(
    3, [point([0, 0]), point([3, 3])]
)

a2 = arc(
    3, [point([1, 2]), point([4, -1])]
)

a3 = arc(
    3, [point([3, 3]), point([6, 0])]
)

a4 = arc(
    3, [point([4, 3]), point([7, 0])]
)

a5 = arc(
    2, [point([0, 1]), point([2, 3])]
)


print("testing with double intersections")
a1.intersection_with_arc(a2)
print("testing with single intersection")
a1.intersection_with_arc(a3)
print("testing with no intersection (too far)")
a1.intersection_with_arc(a4)
print("testing with no intersection (same center)")
a1.intersection_with_arc(a5)

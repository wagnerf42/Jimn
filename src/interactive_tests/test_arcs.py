#!/usr/bin/env python3

from jimn.point import point
from jimn.arc import arc
from jimn.displayable import tycat
from jimn.coordinates_hash import coordinates_hash

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

rounder = coordinates_hash(2)
print("testing with double intersections")
i = a1.intersection_with_arc(a2, rounder)
tycat(a1, a2, i)
print("testing with single intersection")
i = a1.intersection_with_arc(a3, rounder)
tycat(a1, a3, i)
print("testing with no intersection (too far)")
i = a1.intersection_with_arc(a4, rounder)
tycat(a1, a4, i)
print("testing with no intersection (same center)")
i = a1.intersection_with_arc(a5, rounder)
tycat(a1, a5, i)

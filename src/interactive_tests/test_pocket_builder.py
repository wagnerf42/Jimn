#!/usr/bin/env python

from jimn.displayable import tycat
from jimn.pocket.builder import build_pockets
from jimn.point import point
from jimn.segment import segment

a = point([0.0, 3.0])
b = point([0.0, 0.0])
c = point([3.0, 0.0])
d = point([6.0, 0.0])
e = point([6.0, 3.0])

pockets = build_pockets(
    [
        segment([a, b]),
        segment([a, c]),
        segment([b, c]),
        segment([c, d]),
        segment([c, e]),
        segment([d, e]),
    ]
)

print("two pockets connected by one point")
print("should create two different pockets")
tycat(*pockets)

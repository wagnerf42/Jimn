#!/usr/bin/env python3

from jimn.point import point
from jimn.segment import segment
from jimn.arc import arc
from jimn.pocket import pocket
from jimn.displayable import tycat
from jimn.pocket.builder import build_pockets

print("test to follow all paths in a given pocket, building sub-pockets")
p = pocket([
    segment([point([0.0, -0.5]), point([0.5, -0.5])]),
    segment([point([0.5, 1.0]), point([0.0, 1.0])]),
    segment([point([1.0, 0.0]), point([1.0, 0.5])]),
    segment([point([-0.5, 0.5]), point([-0.5, 0.0])]),
    arc(1, [point([-0.5, 0.5]), point([-0.46824583655185426, 0.25])],
        point([0.5, 0.5]), True),
    arc(1, [point([-0.46824583655185426, 0.25]),
            point([0.25, -0.46824583655185426])], point([0.5, 0.5]), True),
    arc(1, [point([0.25, -0.46824583655185426]),
            point([0.5, -0.5])], point([0.5, 0.5]), True),
    arc(1, [point([0.5, 1.0]), point([0.25, 0.9682458365518543])],
        point([0.5, 0.0]), True),
    arc(1, [point([0.25, 0.9682458365518543]),
            point([-0.46824583655185426, 0.25])], point([0.5, 0.0]), True),
    arc(1, [point([-0.46824583655185426, 0.25]), point([-0.5, 0.0])],
        point([0.5, 0.0]), True),
    arc(1, [point([1.0, 0.0]), point([0.9682458365518543, 0.25])],
        point([0.0, 0.0]), True),
    arc(1, [point([0.9682458365518543, 0.25]),
            point([0.25, 0.9682458365518543])], point([0.0, 0.0]), True),
    arc(1, [point([0.25, 0.9682458365518543]),
            point([0.0, 1.0])], point([0.0, 0.0]), True),
    arc(1, [point([0.0, -0.5]), point([0.25, -0.46824583655185426])],
        point([0.0, 0.5]), True),
    arc(1, [point([0.25, -0.46824583655185426]),
            point([0.9682458365518543, 0.25])], point([0.0, 0.5]), True),
    arc(1, [point([0.9682458365518543, 0.25]), point([1.0, 0.5])],
        point([0.0, 0.5]), True),
])

tycat(p)

sub_pockets = build_pockets(p.get_content())
print("results")
tycat(*sub_pockets)


print("two pockets connected by one point")
print("should create two different pockets")
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

tycat(*pockets)

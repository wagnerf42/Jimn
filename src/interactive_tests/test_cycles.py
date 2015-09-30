#!/usr/bin/env python3

from jimn.point import point
from jimn.segment import segment
from jimn.displayable import tycat
from jimn.path import path

p = path([
    segment([point([0, 0]), point([5, 0])]),
    segment([point([5, 0]), point([5, 6])]),
    segment([point([5, 6]), point([1, 6])]),
    segment([point([1, 6]), point([0, 0])]),
])

print("initial path")
tycat(p, p.get_start())
print("setting same starting point")
p.change_starting_point(point([0, 0]))
tycat(p, p.get_start())
print("setting start to a real point")
p.change_starting_point(point([5, 6]))
tycat(p, p.get_start())
print("setting start to a non-real point")
p.change_starting_point(point([2, 0]))
tycat(p, p.get_start())

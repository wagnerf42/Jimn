#!/usr/bin/env python3

from jimn.point import Point
from jimn.arc import Arc
from jimn.displayable import tycat
from math import sqrt

print("this files test reversed arcs")
print("first create and display some")

a = Arc(1, [Point([-1, 0]), Point([0, -1])], Point([0, 0]))
print("original arc")
tycat(a, a.center, *a.endpoints)

b = a.reverse()
print("reversed arc")
tycat(b, b.center, *b.endpoints)

print("now split them at midpoint")
midpoint = Point([-sqrt(2)/2, -sqrt(2)/2])

print("start with original")
split_a = a.split_at([midpoint])
points = [p for s in split_a for p in s.endpoints]
tycat(*split_a, *points)

print("continue with reversed")
split_b = b.split_at([midpoint])
points = [p for s in split_b for p in s.endpoints]
tycat(*split_b, *points)

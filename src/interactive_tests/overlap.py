#!/usr/bin/env python3

from jimn.point import Point
from jimn.segment import Segment
from jimn.displayable import tycat

print("**************************")
print("*testing segments overlap*")
print("**************************")

print("non aligned")

s1 = Segment([Point([0, 3]), Point([0, 6])])
s2 = Segment([Point([1, 3]), Point([0, 6])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)

assert r is None

print("aligned, no overlap")

s1 = Segment([Point([0, 0]), Point([3, 3])])
s2 = Segment([Point([4, 4]), Point([6, 6])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)

assert r is None

print("overlap, no one disappears")

s1 = Segment([Point([0, 0]), Point([3, 3])])
s2 = Segment([Point([2, 2]), Point([6, 6])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)
tycat(*r)

print("overlap, no one disappears ; different directions")

s1 = Segment([Point([0, 0]), Point([3, 3])])
s2 = Segment([Point([6, 6]), Point([2, 2])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)
tycat(*r)

print("overlap, no one diappears ; again different directions")

s1 = Segment([Point([3, 3]), Point([0, 0])])
s2 = Segment([Point([2, 2]), Point([6, 6])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)
tycat(*r)

print("overlap, one completely contained")

s1 = Segment([Point([0, 0]), Point([6, 6])])
s2 = Segment([Point([2, 2]), Point([3, 3])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)
tycat(*r)

print("overlap, one completely contained ; different directions")

s1 = Segment([Point([0, 0]), Point([6, 6])])
s2 = Segment([Point([3, 3]), Point([2, 2])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)
tycat(*r)

print("overlap, one completely contained ; different directions")

s1 = Segment([Point([6, 6]), Point([0, 0])])
s2 = Segment([Point([0, 0]), Point([2, 2])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)
tycat(*r)

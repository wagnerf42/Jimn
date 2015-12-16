#!/usr/bin/env python3

from jimn.point import point
from jimn.segment import segment
from jimn.displayable import tycat

print("**************************")
print("*testing segments overlap*")
print("**************************")

print("non aligned")

s1 = segment([point([0, 3]), point([0, 6])])
s2 = segment([point([1, 3]), point([0, 6])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)

assert r is None

print("aligned, no overlap")

s1 = segment([point([0, 0]), point([3, 3])])
s2 = segment([point([4, 4]), point([6, 6])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)

assert r is None

print("overlap, no one disappears")

s1 = segment([point([0, 0]), point([3, 3])])
s2 = segment([point([2, 2]), point([6, 6])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)
tycat(*r)

print("overlap, no one disappears ; different directions")

s1 = segment([point([0, 0]), point([3, 3])])
s2 = segment([point([6, 6]), point([2, 2])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)
tycat(*r)

print("overlap, no one diappears ; again different directions")

s1 = segment([point([3, 3]), point([0, 0])])
s2 = segment([point([2, 2]), point([6, 6])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)
tycat(*r)

print("overlap, one completely contained")

s1 = segment([point([0, 0]), point([6, 6])])
s2 = segment([point([2, 2]), point([3, 3])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)
tycat(*r)

print("overlap, one completely contained ; different directions")

s1 = segment([point([0, 0]), point([6, 6])])
s2 = segment([point([3, 3]), point([2, 2])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)
tycat(*r)

print("overlap, one completely contained ; different directions")

s1 = segment([point([6, 6]), point([0, 0])])
s2 = segment([point([0, 0]), point([2, 2])])
tycat(s1, s2)

r = s1.remove_overlap_with(s2)
tycat(*r)

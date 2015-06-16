#!/usr/bin/env python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.displayable import tycat

a = point([0.0, 0.0])
b = point([9.0, 0.0])
c = point([9.0, 8.0])
d = point([0.0, 8.0])

e = point([1.0, 5.0])
f = point([4.0, 3.0])
g = point([8.0, 5.0])
h = point([4.0, 7.0])

i = point([2.0, 4.5])
j = point([3.0, 4.5])
k = point([3.0, 5.5])
l = point([2.0, 5.5])

m = point([5.0, 4.5])
n = point([6.0, 4.5])
o = point([6.0, 5.5])
p = point([5.0, 5.5])

q = point([6.0, 1.0])
r = point([7.0, 1.0])
s = point([7.0, 2.0])
t = point([6.0, 2.0])

points1 = [a, b, c, d]
points4 = [e, f, g, h]
points5 = [i, j, k, l]
points6 = [m, n, o, p]
points7 = [q, r, s, t]


poly1 = polygon(list(points1))

poly2 = polygon(list(points1))

poly3 = polygon(list(points1))
poly4 = polygon(list(points4))
poly5 = polygon(list(points5))
poly6 = polygon(list(points6))
poly7 = polygon(list(points7))

poly8 = polygon(list(points1))
poly9 = polygon(list(points5))
poly10 = polygon(list(points4))

tycat(poly1)
tycat(poly2)
tycat(poly3, poly4, poly5, poly6, poly7)
tycat(poly8, poly9, poly10)

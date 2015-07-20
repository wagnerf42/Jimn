#!/usr/bin/python3

from jimn.point import point
from jimn.segment import segment
from jimn.polygon import polygon
from jimn.holed_polygon import holed_polygon
from jimn.displayable import tycat


a = point([0.0, 4.0])
b = point([1.0, 8.0])
c = point([2.0, 4.0])
d = point([3.0, 8.0])
e = point([4.0, 8.0])
f = point([5.0, 12.0])
g = point([8.0, 12.0])
h = point([9.0, 8.0])
i = point([10.0, 8.0])
j = point([11.0, 4.0])
k = point([12.0, 8.0])
l = point([13.0, 8.0])
m = point([13.0, 0.0])
n = point([0.0, 0.0])

poly = polygon([a, b, c, d, e, f, g, h, i, j, k, l, m, n])

o = point([6.0, 4.0])
p = point([6.0, 8.0])
q = point([7.0, 8.0])
r = point([7.0, 4.0])

hole = polygon([o, p, q, r])

hp = holed_polygon(poly, height=0, holes=[hole])

heights = [4.0 * u for u in range(0, 4)]
center_lines = [segment([point([0.0, y]), point([13.0, y])]) for y in heights]

g = hp.build_graph(4.0)

for v in sorted(g.get_vertices(), key=lambda v: (v.get_y(), v.get_x())):
    print([str(edge) for edge in v.edges])
    tycat(center_lines, hp, v, *(v.edges))

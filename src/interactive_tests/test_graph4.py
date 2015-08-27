#!/usr/bin/python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.pocket import pocket
from jimn.displayable import tycat, tycat_set_svg_dimensions
from jimn.graph.eulerian_cycle import find_eulerian_cycle

tycat_set_svg_dimensions(640, 480)

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

segments = list(polygon([a, b, c, d, e, f, g, h, i, j, k, l, m, n]).segments())
segments.extend(list(polygon([o, p, q, r]).segments()))  # hole
hp = pocket(segments)

print("complex case for internal edges")
g = hp.build_graph(4.0)
tycat(g)
p = find_eulerian_cycle(g)
p.animate(hp)

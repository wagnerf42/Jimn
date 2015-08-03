#!/usr/bin/python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.holed_polygon import holed_polygon
from jimn.displayable import tycat, tycat_set_svg_dimensions

tycat_set_svg_dimensions(640, 480)

a = point([0.5, 0.1])
b = point([0.0, 1.1])
c = point([1.0, 1.1])
d = point([0.5, 0.4])
e = point([0.2, 1.0])
f = point([0.8, 1.0])

abc = holed_polygon(polygon([a, b, c]), height=0, holes=[polygon([d, e, f])])

g = abc.build_graph(0.3)
tycat(g)
p = g.find_eulerian_cycle()
p.animate(abc)

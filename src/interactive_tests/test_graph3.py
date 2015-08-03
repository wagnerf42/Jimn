#!/usr/bin/python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.holed_polygon import holed_polygon
from jimn.displayable import tycat, tycat_set_svg_dimensions

tycat_set_svg_dimensions(640, 480)

a = point([0.6, 0.0])
b = point([0.0, 0.3])
c = point([1.2, 0.3])

abc = holed_polygon(polygon([a, b, c]))

print("some more extreme graphs")
g = abc.build_graph(0.3)
tycat(g)
p = g.find_eulerian_cycle()
p.animate(abc)

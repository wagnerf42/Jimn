#!/usr/bin/python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.holed_polygon import holed_polygon
from jimn.displayable import tycat, tycat_set_svg_dimensions
from jimn.graph.eulerian_cycle import find_eulerian_cycle

tycat_set_svg_dimensions(640, 480)

a = point([0.6, 0.0])
b = point([0.0, 1.2])
c = point([1.2, 1.2])

abc = holed_polygon(polygon([a, b, c]))

print("top vertex is lying on a cut line")
g = abc.build_graph(0.3)
tycat(g)
p = find_eulerian_cycle(g)
p.animate(abc)

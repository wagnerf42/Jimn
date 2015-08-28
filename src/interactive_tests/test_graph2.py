#!/usr/bin/python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.pocket import pocket
from jimn.pocket.graph_builder import build_graph
from jimn.displayable import tycat, tycat_set_svg_dimensions
from jimn.graph.eulerian_cycle import find_eulerian_cycle

tycat_set_svg_dimensions(640, 480)

a = point([0.6, 0.0])
b = point([0.0, 1.2])
c = point([1.2, 1.2])

segments = list(polygon([a, b, c]).segments())  # outer edge
abc = pocket(segments)

print("top vertex is lying on a cut line")
g = build_graph(abc, 0.3)
tycat(g)
p = find_eulerian_cycle(g)
p.animate(abc)

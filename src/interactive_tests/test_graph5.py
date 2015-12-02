#!/usr/bin/env python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.displayable import tycat, tycat_set_svg_dimensions
from jimn.pocket import pocket
from jimn.pocket.graph_builder import build_graph
from jimn.graph.eulerian_cycle import find_eulerian_cycle, cycle_to_path

print("tries fast algorithm for even edges")
print("you can clearly see it is not optimal")

tycat_set_svg_dimensions(640, 480)

a = point([0.0, 0.0])
b = point([-5, 0.5])
c = point([0.0, 1.0])
d = point([0.0, 4.0])
e = point([4.0, 4.0])
f = point([4.0, 0.0])

segments = list(polygon([a, b, c, d, e, f]).segments())  # outer edge
abc = pocket(segments)

g = build_graph(abc, 1, True)
tycat(g)
p = cycle_to_path(find_eulerian_cycle(g))
p.animate()

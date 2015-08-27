#!/usr/bin/python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.displayable import tycat, tycat_set_svg_dimensions
from jimn.pocket import pocket
from jimn.graph.eulerian_cycle import find_eulerian_cycle

tycat_set_svg_dimensions(640, 480)

a = point([0.5, 0.1])
b = point([0.0, 1.1])
c = point([1.0, 1.1])
d = point([0.5, 0.4])
e = point([0.2, 1.0])
f = point([0.8, 1.0])

segments = list(polygon([a, b, c]).segments())  # outer edge
segments.extend(list(polygon([d, e, f]).segments()))  # hole
abc = pocket(segments)

g = abc.build_graph(0.3)
tycat(g)
p = find_eulerian_cycle(g)
p.animate(abc)

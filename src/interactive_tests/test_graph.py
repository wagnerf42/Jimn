#!/usr/bin/python3

from jimn.point import point
from jimn.vertex import vertex
from jimn.segment import segment
from jimn.polygon import polygon
from jimn.holed_polygon import holed_polygon
from jimn.displayable import tycat


a = point([0.5, 0.1])
b = point([0.0, 1.1])
c = point([1.0, 1.1])

abc = holed_polygon(polygon([a, b, c]))

heights = [0.3 * n for n in range(0, 5)]
center_lines = [segment([point([0.0, h]), point([1.2, h])]) for h in heights]

points = abc.build_graph(0.3)

for p in points:
    if type(p) is vertex:
        tycat(center_lines, abc, p, *(p.links))
    else:
        tycat(center_lines, abc, p)

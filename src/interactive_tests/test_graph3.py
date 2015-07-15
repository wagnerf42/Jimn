#!/usr/bin/python3

from jimn.point import point
from jimn.segment import segment
from jimn.polygon import polygon
from jimn.polygon import NoVertex
from jimn.holed_polygon import holed_polygon
from jimn.displayable import tycat


a = point([0.6, 0.0])
b = point([0.0, 0.3])
c = point([1.2, 0.3])

abc = holed_polygon(polygon([a, b, c]))

heights = [0.3 * n for n in range(0, 5)]
center_lines = [segment([point([0.0, h]), point([1.2, h])]) for h in heights]

tycat(center_lines, abc)

try:
    points = abc.build_graph(0.3)
except NoVertex:
    print("NoVertex exception raised : ok")

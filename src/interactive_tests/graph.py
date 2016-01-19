#!/usr/bin/python3

from jimn.point import Point
from jimn.polygon import Polygon
from jimn.displayable import tycat, tycat_set_svg_dimensions
from jimn.pocket import pocket
from jimn.holed_pocket import HoledPocket
from jimn.pocket.graph_builder import build_graph
from jimn.graph.eulerian_cycle import find_eulerian_cycle, cycle_to_path

tycat_set_svg_dimensions(640, 480)

print("small graph with hole")

a = Point([0.5, 0.1])
b = Point([0.0, 1.1])
c = Point([1.0, 1.1])
d = Point([0.5, 0.4])
e = Point([0.2, 1.0])
f = Point([0.8, 1.0])

outer_edge = pocket(list(Polygon([a, b, c]).segments()))
hole = pocket(list(Polygon([d, e, f]).segments()))
abc = HoledPocket(outer_edge, [hole])

g = build_graph(abc, 0.3)
tycat(g)
p = cycle_to_path(find_eulerian_cycle(g))
p.animate(0.3)

print("top vertex is lying on a cut line")

a = Point([0.6, 0.0])
b = Point([0.0, 1.2])
c = Point([1.2, 1.2])

segments = list(Polygon([a, b, c]).segments())  # outer edge
abc = HoledPocket(pocket(segments))

g = build_graph(abc, 0.3)
tycat(g)
p = cycle_to_path(find_eulerian_cycle(g))
p.animate(0.3)

print("some more extreme graphs")
a = Point([0.6, 0.0])
b = Point([0.0, 0.3])
c = Point([1.2, 0.3])

segments = list(Polygon([a, b, c]).segments())  # outer edge
abc = HoledPocket(pocket(segments))

g = build_graph(abc, 0.3)
tycat(g)
p = cycle_to_path(find_eulerian_cycle(g))
p.animate(0.3)

print("complex case for internal edges")
a = Point([0.0, 4.0])
b = Point([1.0, 8.0])
c = Point([2.0, 4.0])
d = Point([3.0, 8.0])
e = Point([4.0, 8.0])
f = Point([5.0, 12.0])
g = Point([8.0, 12.0])
h = Point([9.0, 8.0])
i = Point([10.0, 8.0])
j = Point([11.0, 4.0])
k = Point([12.0, 8.0])
l = Point([13.0, 8.0])
m = Point([13.0, 0.0])
n = Point([0.0, 0.0])

poly = Polygon([a, b, c, d, e, f, g, h, i, j, k, l, m, n])

o = Point([6.0, 4.0])
p = Point([6.0, 8.0])
q = Point([7.0, 8.0])
r = Point([7.0, 4.0])

hole = Polygon([o, p, q, r])
hp = HoledPocket(
    pocket(
        list(Polygon([a, b, c, d, e, f, g, h, i, j, k, l, m, n]).segments())
    ),
    [pocket(list(Polygon([o, p, q, r]).segments()))]
)

g = build_graph(hp, 4.0)
tycat(g)
p = cycle_to_path(find_eulerian_cycle(g))
p.animate(4.0)


print("tries fast algorithm for even edges")

a = Point([0.0, 0.0])
b = Point([-5, 0.5])
c = Point([0.0, 1.0])
d = Point([0.0, 4.0])
e = Point([4.0, 4.0])
f = Point([4.0, 0.0])

segments = list(Polygon([a, b, c, d, e, f]).segments())  # outer edge
abc = HoledPocket(pocket(segments))

g = build_graph(abc, 1, True)
tycat(g)
p = cycle_to_path(find_eulerian_cycle(g))
p.animate(1)

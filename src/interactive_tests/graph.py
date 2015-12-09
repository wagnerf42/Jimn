#!/usr/bin/python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.displayable import tycat, tycat_set_svg_dimensions
from jimn.pocket import pocket
from jimn.holed_pocket import holed_pocket
from jimn.pocket.graph_builder import build_graph
from jimn.graph.eulerian_cycle import find_eulerian_cycle, cycle_to_path

tycat_set_svg_dimensions(640, 480)

print("small graph with hole")

a = point([0.5, 0.1])
b = point([0.0, 1.1])
c = point([1.0, 1.1])
d = point([0.5, 0.4])
e = point([0.2, 1.0])
f = point([0.8, 1.0])

outer_edge = pocket(list(polygon([a, b, c]).segments()))
hole = pocket(list(polygon([d, e, f]).segments()))
abc = holed_pocket(outer_edge, [hole])

g = build_graph(abc, 0.3)
tycat(g)
p = cycle_to_path(find_eulerian_cycle(g))
p.animate(abc)

print("top vertex is lying on a cut line")

a = point([0.6, 0.0])
b = point([0.0, 1.2])
c = point([1.2, 1.2])

segments = list(polygon([a, b, c]).segments())  # outer edge
abc = holed_pocket(pocket(segments))

g = build_graph(abc, 0.3)
tycat(g)
p = cycle_to_path(find_eulerian_cycle(g))
p.animate(abc)

print("some more extreme graphs")
a = point([0.6, 0.0])
b = point([0.0, 0.3])
c = point([1.2, 0.3])

segments = list(polygon([a, b, c]).segments())  # outer edge
abc = holed_pocket(pocket(segments))

g = build_graph(abc, 0.3)
tycat(g)
p = cycle_to_path(find_eulerian_cycle(g))
p.animate(abc)

print("complex case for internal edges")
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
hp = holed_pocket(
    pocket(
        list(polygon([a, b, c, d, e, f, g, h, i, j, k, l, m, n]).segments())
    ),
    [pocket(list(polygon([o, p, q, r]).segments()))]
)

g = build_graph(hp, 4.0)
tycat(g)
p = cycle_to_path(find_eulerian_cycle(g))
p.animate(hp)


print("tries fast algorithm for even edges")

a = point([0.0, 0.0])
b = point([-5, 0.5])
c = point([0.0, 1.0])
d = point([0.0, 4.0])
e = point([4.0, 4.0])
f = point([4.0, 0.0])

segments = list(polygon([a, b, c, d, e, f]).segments())  # outer edge
abc = holed_pocket(pocket(segments))

g = build_graph(abc, 1, True)
tycat(g)
p = cycle_to_path(find_eulerian_cycle(g))
p.animate()

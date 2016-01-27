#!/usr/bin/env python3

from jimn.point import Point
from jimn.polygon import Polygon
from jimn.displayable import tycat
from jimn.tree.polygon_tree import PolygonTree
from jimn.utils.debug import add_module_to_debug

a = Point([0.0, 0.0])
b = Point([9.0, 0.0])
c = Point([9.0, 8.0])
d = Point([0.0, 8.0])

e = Point([1.0, 5.0])
f = Point([4.0, 3.0])
g = Point([8.0, 5.0])
h = Point([4.0, 7.0])

i = Point([2.0, 4.5])
j = Point([3.0, 4.5])
k = Point([3.0, 5.5])
l = Point([2.0, 5.5])

m = Point([5.0, 4.5])
n = Point([6.0, 4.5])
o = Point([6.0, 5.5])
p = Point([5.0, 5.5])

q = Point([6.0, 1.0])
r = Point([7.0, 1.0])
s = Point([7.0, 2.0])
t = Point([6.0, 2.0])

points1 = [a, b, c, d]
points4 = [e, f, g, h]
points5 = [i, j, k, l]
points6 = [m, n, o, p]
points7 = [q, r, s, t]


polygons = {}

height = 4
poly1 = Polygon(list(points1), 1)
polygons[height] = [poly1]

height -= 1
poly2 = Polygon(list(points1), 2)
polygons[height] = [poly2]

height -= 1
poly3 = Polygon(list(points1), 3)
poly4 = Polygon(list(points4), 4)
poly5 = Polygon(list(points5), 5)
poly6 = Polygon(list(points6), 6)
poly7 = Polygon(list(points7), 7)
polygons[height] = [poly3, poly4, poly5, poly6, poly7]

height -= 1
poly8 = Polygon(list(points1), 8)
poly9 = Polygon(list(points5), 9)
poly10 = Polygon(list(points4), 10)
polygons[height] = [poly8, poly9, poly10]

tycat(poly1)
tycat(poly2)
tycat(poly3, poly4, poly5, poly6, poly7)
tycat(poly8, poly9, poly10)

add_module_to_debug("jimn.algorithms.sweeping_line_algorithms.inclusion_tree_builder")
add_module_to_debug("jimn.tree.polygon_tree")
tree = PolygonTree.build(0.01, polygons)
print("simulating depth first carving")
tree.display_depth_first()

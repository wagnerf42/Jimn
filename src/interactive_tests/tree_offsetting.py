#!/usr/bin/env python3

from jimn.polygon import Polygon
from jimn.displayable import tycat
from jimn.tree.polygon_tree import PolygonTree
from jimn.tree.pocket_tree import PocketTree

polygons = {}
polygons[2] = [Polygon.square(0, 0, 10)]
polygons[1] = [
    Polygon.square(0, 0, 10),
    Polygon.square(1, 1, 3),
    Polygon.square(5, 1, 3),
    Polygon.square(5.5, 1.5, 2)
]

polygons[0] = [
    Polygon.square(0, 0, 10),
    Polygon.square(5, 1, 3),
    Polygon.square(5.5, 1.5, 0.5),
    Polygon.square(7, 3, 0.5)
]

print("layouts are")
tycat(polygons[2])
tycat(polygons[1])
tycat(polygons[0])

print("corresponding tree")
tree = PolygonTree.build(0.1, polygons)
tree.tycat()

pockets = PocketTree.build(tree, 0.1)
print("pockets tree")
pockets.tycat()
pockets.display_breadth_first()

#!/usr/bin/env python3

from jimn.polygon import Polygon
from jimn.displayable import tycat
from jimn.tree.polygon_tree import PolygonTree
from jimn.tree.pocket_tree import PocketTree
from jimn.tree.path_tree import path_tree

milling_radius = 0.9

polygons = {}
polygons[1] = [Polygon.square(0, 0, 10)]
polygons[0] = [
    Polygon.square(0, 0, 10),
    Polygon.square(2, 2, 6),
    Polygon.square(3.55, 4.55, 3),
]

print("layouts are")
tycat(polygons[1])
tycat(polygons[0])

tree = PolygonTree.build(milling_radius, polygons)
print("corresponding tree")
tree.tycat()

pockets = PocketTree.build(tree, milling_radius)
print("offsetted tree")
pockets.tycat()

paths = path_tree.build(pockets, milling_radius)
print("path tree")
paths.tycat()
paths.animate(milling_radius)

global_path = paths.global_path(milling_radius)
global_path.animate(milling_radius)
#print("total length:", global_path.length())

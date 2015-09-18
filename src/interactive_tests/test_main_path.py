#!/usr/bin/env python3

from jimn.polygon import polygon
from jimn.displayable import tycat
from jimn.tree.polygon_tree import polygon_tree
from jimn.tree.pocket_tree import pocket_tree
from jimn.tree.path_tree import path_tree

milling_radius = 0.5
# milling_radius = 0.1 #bug

polygons = {}
polygons[1] = [polygon.square(0, 0, 10)]
polygons[0] = [
    polygon.square(0, 0, 10),
    polygon.square(3.55, 4.55, 3),
]

print("layouts are")
tycat(polygons[1])
tycat(polygons[0])

print("corresponding tree")
tree = polygon_tree.build(polygons)
tree.tycat()

pockets = pocket_tree.build(tree, milling_radius)
print("offsetted tree")
pockets.tycat()

paths = path_tree.build(pockets, milling_radius)
#paths.animate()
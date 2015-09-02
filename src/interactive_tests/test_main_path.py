#!/usr/bin/env python3

from jimn.polygon import polygon
from jimn.displayable import tycat
from jimn.polygontree.polygontree_builder import build_tree

polygons = {}
polygons[1] = [polygon.square(0, 0, 10)]
polygons[0] = [
    polygon.square(0, 0, 10),
    polygon.square(5.55, 4.55, 4),
]

print("layouts are")
tycat(polygons[1])
tycat(polygons[0])

print("corresponding tree")
tree = build_tree(polygons)
tree.tycat()

offsetted_tree = tree.offset_polygons(0.1)
print("offsetted tree")
offsetted_tree.tycat()
path = offsetted_tree.compute_path()

#!/usr/bin/env python3

from jimn.polygon import polygon
from jimn.displayable import tycat
from jimn.tree.polygon_tree import polygon_tree
from jimn.tree.pocket_tree import pocket_tree

polygons = {}
polygons[2] = [polygon.square(0, 0, 10)]
polygons[1] = [
    polygon.square(0, 0, 10),
    polygon.square(1, 1, 3),
    polygon.square(5, 1, 3),
    polygon.square(5.5, 1.5, 2)
]

polygons[0] = [
    polygon.square(0, 0, 10),
    polygon.square(5, 1, 3),
    polygon.square(5.5, 1.5, 0.5),
    polygon.square(7, 3, 0.5)
]

print("layouts are")
tycat(polygons[2])
tycat(polygons[1])
tycat(polygons[0])

print("corresponding tree")
tree = polygon_tree.build(0.1, polygons)
tree.tycat()

pockets = pocket_tree.build(tree, 0.1)
print("pockets tree")
pockets.tycat()
pockets.display_breadth_first()

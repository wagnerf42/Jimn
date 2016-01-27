#!/usr/bin/env python3

from jimn.polygon import Polygon
from jimn.algorithms.offsetter import offset_holed_polygon
from jimn.pocket.graph_builder import build_graph
from jimn.graph.eulerian_cycle import find_eulerian_cycle, cycle_to_path
from jimn.tree.path_tree import PathTree

polygons = [
    Polygon.square(0, 0, 12),
    Polygon.square(2.5, 2.5, 3),
    Polygon.square(6.5, 6.5, 3),
    Polygon.square(0.1, 0.1, 4.1),
    Polygon.square(0.1, 0.1, 4.1),
    Polygon.square(0.1, 0.1, 4.1),
]

for p in polygons:
    p.orient(False)

pockets = [offset_holed_polygon(1, p)[0] for p in polygons]
graphs = [build_graph(p, 1) for p in pockets]
paths = [cycle_to_path(find_eulerian_cycle(g)) for g in graphs]
trees = [PathTree(paths[i], pockets[i].outer_edge) for i in range(len(paths))]

print("fully overlapping")
t = PathTree()
t.children = [trees[3]]
trees[3].children = [trees[4]]
trees[4].children = [trees[5]]
t.tycat()
result = t.global_path(1)
result.animate(1)

print("not fully overlapping")
t2 = PathTree()
t2.children = [trees[0]]
trees[0].children = [trees[1], trees[2]]
t2.tycat()
result = t2.global_path(1)
result.animate(1)

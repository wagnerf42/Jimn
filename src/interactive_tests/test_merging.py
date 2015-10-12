#!/usr/bin/env python3

from jimn.polygon import polygon
from jimn.algorithms.offsetter import offset_holed_polygon
from jimn.pocket.graph_builder import build_graph
from jimn.graph.eulerian_cycle import find_eulerian_cycle, cycle_to_path
from jimn.tree.path_tree.path_merger import overlap_exit_position, merge_path

p1 = polygon.square(0, 0, 10)
p1.orient(False)
p2 = polygon.square(3, 3, 5)
p2.orient(False)
pockets1 = offset_holed_polygon(1, p1)
pockets2 = offset_holed_polygon(1, p2)
g1 = build_graph(pockets1[0], 1)
g2 = build_graph(pockets2[0], 1)
path1 = cycle_to_path(find_eulerian_cycle(g1))
path2 = cycle_to_path(find_eulerian_cycle(g2))

pos = overlap_exit_position(path1, path2, 1)
path1.animate(pos.outer_point, pos.inner_point, path2)
merge_path(path1, path2, pos)
path1.animate()

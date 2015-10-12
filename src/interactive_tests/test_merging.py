#!/usr/bin/env python3

from jimn.polygon import polygon
from jimn.algorithms.offsetter import offset_holed_polygon
from jimn.pocket.graph_builder import build_graph
from jimn.graph.eulerian_cycle import find_eulerian_cycle, cycle_to_path
from jimn.tree.path_tree.path_merger import overlap_exit_position, merge_path

polygons = [
    polygon.square(0, 0, 12),
    polygon.square(2.5, 2.5, 3),
    polygon.square(6.5, 6.5, 3)
]

for p in polygons:
    p.orient(False)

pockets = [offset_holed_polygon(1, p) for p in polygons]
graphs = [build_graph(p[0], 1) for p in pockets]
paths = [cycle_to_path(find_eulerian_cycle(g)) for g in graphs]

pos1 = overlap_exit_position(paths[0], paths[1], 1)
pos2 = overlap_exit_position(paths[0], paths[2], 1)

if pos1 > pos2:
    merge_path(paths[0], paths[1], pos1)
    merge_path(paths[0], paths[2], pos2)
else:
    merge_path(paths[0], paths[2], pos2)
    merge_path(paths[0], paths[1], pos1)

paths[0].animate()

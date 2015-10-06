#!/usr/bin/env python3

from jimn.point import point
from jimn.graph import graph
from jimn.graph.tsp import min_spanning_tree
from jimn.displayable import tycat

points = ([
    point([0, 0]),
    point([1, 1]),
    point([2, 4]),
    point([2, 7]),
    point([3, 5]),
    point([3, 4]),
    point([3, 0]),
])

g = graph.complete_graph(points)
edges = min_spanning_tree(g)
tycat(g, edges)

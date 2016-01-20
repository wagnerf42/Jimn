#!/usr/bin/env python3

from jimn.point import Point
from jimn.graph import Graph
from jimn.graph.tsp import tsp
from jimn.displayable import tycat
from jimn.utils.debug import add_module_to_debug

add_module_to_debug("jimn.graph.tsp")


points = ([
    Point([0, 0]),
    Point([1, 1]),
    Point([2, 4]),
    Point([2, 7]),
    Point([3, 5]),
    Point([3, 4]),
    Point([3, 0]),
])

g = Graph.complete_graph(points)
tycat(g)
tsp(g)

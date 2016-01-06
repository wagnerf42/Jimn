#!/usr/bin/env python3

from jimn.point import point
from jimn.graph import graph
from jimn.graph.tsp import tsp
from jimn.displayable import tycat
from jimn.utils.debug import add_module_to_debug

add_module_to_debug("jimn.graph.tsp")


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
tycat(g)
tsp(g)
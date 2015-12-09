"""
build graph out of a pocket (external edges following pocket's edge
and internal edges etching pocket's surface)
"""


def build_graph(milled_pocket, milling_diameter, fast_algorithm=False):
    """
    returns graph which will be used to compute milling path
    """
    if __debug__:
        if is_module_debugged(__name__):
            print("creating graph out of pocket")
            tycat(milled_pocket)

    # fill all vertices
    g = graph()
    _create_vertices(milled_pocket, milling_diameter, g)

    # finish by adding horizontal internal edges
    create_internal_edges(g, milling_diameter)
    if __debug__:
        if is_module_debugged(__name__):
            print("created internal edges")
            tycat(g)

    # prepare for eulerian path
    if fast_algorithm:
        make_degrees_even_fast(g, milling_diameter)
    else:
        make_degrees_even(g)

    if __debug__:
        if is_module_debugged(__name__):
            print("degrees made even")
            tycat(g)

    return g


def _create_vertices(milled_pocket, milling_diameter, built_graph):
    # first cut by horizontal lines spaced by milling_diameter
    split_pocket = milled_pocket.split_at_milling_points(milling_diameter)
    # ok, now create graph, each segment point becomes a vertex
    # and we add all external edges
    for s in split_pocket.paths:
        built_graph.add_edge(s, frontier_edge=True)

    if __debug__:
        if is_module_debugged(__name__):
            print("created vertices")
            tycat(built_graph)


from jimn.displayable import tycat
from jimn.graph import graph
from jimn.graph.even_degrees import make_degrees_even, make_degrees_even_fast
from jimn.graph.internal_edges import create_internal_edges
from jimn.utils.debug import is_module_debugged

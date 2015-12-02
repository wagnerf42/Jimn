from jimn.pocket.elementary_paths import pocket_intersect_paths
from jimn.displayable import tycat
from jimn.graph import graph
from jimn.graph.even_degrees import make_degrees_even, make_degrees_even_fast
from jimn.graph.internal_edges import create_internal_edges
from jimn.segment import segment
from jimn.utils.debug import is_module_debugged
from math import floor, ceil


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
    box = milled_pocket.get_bounding_box()
    xmin, xmax = box.limits(0)
    cutting_lines = [
        segment.horizontal_segment(xmin, xmax, y)
        for y in _milling_heights(milled_pocket, milling_diameter)
    ]
    split_pocket = pocket_intersect_paths(milled_pocket, cutting_lines)
    elementary_segments = split_pocket.get_content()
    # ok, now create graph, each segment point becomes a vertex
    # and we add all external edges
    added = []
    for s in elementary_segments:
        added.append(s)
        built_graph.add_edge(s, frontier_edge=True)

    if __debug__:
        if is_module_debugged(__name__):
            print("created vertices")
            tycat(built_graph, cutting_lines)


def _milling_heights(p, milling_diameter):
    box = p.get_bounding_box()
    ymin, ymax = box.limits(1)
    start = floor(ymin / milling_diameter)
    end = ceil(ymax / milling_diameter)
    for i in range(start, end+1):
        yield i * milling_diameter

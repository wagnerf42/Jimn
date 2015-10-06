from jimn.pocket.elementary_paths import pocket_intersect_paths
from jimn.displayable import tycat
from jimn.graph import graph
from jimn.graph.even_degrees import make_degrees_even
from jimn.graph.internal_edges import create_internal_edges
from jimn.segment import segment
from jimn.utils.coordinates_hash import rounder2d
from jimn.utils.debug import is_module_debugged
from math import floor, ceil


def build_graph(milled_pocket, milling_diameter):
    """
    returns graph which will be used to compute milling path
    """
    # round all points on intersecting lines
    for y in _milling_heights(milled_pocket, milling_diameter):
        rounder2d.hash_coordinate(1, y)

    milled_pocket.round_points(rounder2d)

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
    for s in elementary_segments:
        built_graph.add_edge(s, frontier_edge=True)

    if __debug__:
        if is_module_debugged(__name__):
            print("created vertices")
            tycat(built_graph)


def _milling_heights(p, milling_diameter):
    box = p.get_bounding_box()
    ymin, ymax = box.limits(1)
    start = floor(ymin / milling_diameter)
    end = ceil(ymax / milling_diameter)
    for i in range(start, end+1):
        yield i * milling_diameter

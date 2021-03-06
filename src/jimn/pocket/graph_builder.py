"""
build graph out of a pocket (external edges following pocket's edge
and internal edges etching pocket's surface)
"""
from math import ceil, floor
from jimn.displayable import tycat
from jimn.graph import Graph
from jimn.graph.even_degrees import make_degrees_even, make_degrees_even_fast
from jimn.graph.internal_edges import create_internal_edges
from jimn.utils.debug import is_module_debugged
from jimn.utils.math import milling_heights
from jimn.arc import Arc
from jimn.segment import Segment
from jimn.point import Point
from jimn.elementary_path import ElementaryPath
from jimn.utils.precision import is_almost


def build_graph(milled_pocket, milling_diameter, fast_algorithm=False):
    """
    return graph which will be used to compute milling path.
    you can choose between fast (linear) algorithm with good enough quality
    or get the optimal solution (in n^3) using "fast_algorithm" parameter.
    """
    if __debug__:
        if is_module_debugged(__name__):
            print("creating graph out of pocket")
            tycat(milled_pocket)

    # fill all vertices
    graph = Graph()
    _create_vertices(milled_pocket, milling_diameter, graph)

    # finish by adding horizontal internal edges
    create_internal_edges(graph, milling_diameter)
    if __debug__:
        if is_module_debugged(__name__):
            print("created internal edges")
            tycat(graph)

    # prepare for eulerian path
    if fast_algorithm:
        make_degrees_even_fast(graph, milling_diameter)
    else:
        make_degrees_even(graph)

    if __debug__:
        if is_module_debugged(__name__):
            print("degrees made even")
            tycat(graph)

    return graph


def _create_vertices(milled_pocket, milling_diameter, built_graph):
    # first cut by horizontal lines spaced by milling_diameter
    split_pocket = milled_pocket.split_at_milling_points(milling_diameter)
    # ok, now create graph, each segment point becomes a vertex
    # and we add all external edges
    for path in split_pocket.paths:
        built_graph.add_edge(path, frontier_edge=True)

    if __debug__:
        if is_module_debugged(__name__):
            print("created vertices")
            tycat(built_graph)


# enrich arc class
def __split_arcs(self, milling_diameter):
    """
    return array of arcs obtained when stopping at each milling height.
    """
    self.adjust_points_at_milling_height(milling_diameter)
    box = self.get_bounding_box()
    y_limits = box.limits(1)

    points = []
    for milling_y in milling_heights(*y_limits,
                                     milling_diameter=milling_diameter):
        points.extend(self.horizontal_intersections_at(milling_y,
                                                       *box.limits(0)))

    if points:
        return self.split_at(points)
    else:
        return [self]


def __split_segments(self, milling_diameter):
    """
    return array of segments obtained when stopping at each milling height.
    """
    self.adjust_points_at_milling_height(milling_diameter)
    if self.is_almost_horizontal():
        return [self]

    y_limits = [p.get_y() for p in self.endpoints]
    intersections = [
        self.horizontal_intersection_at(y)
        for y in milling_heights(*y_limits,
                                 milling_diameter=milling_diameter)]
    if intersections:
        return self.split_at(intersections)
    else:
        return [self]


def __adjust_point(self, milling_diameter):
    """
    if point is close enough from a milling height, return new point
    exactly at milling height.
    else return point.
    """
    point_y = self.get_y()
    above_height = milling_diameter * floor(point_y/milling_diameter)
    below_height = milling_diameter * ceil(point_y/milling_diameter)
    for height in (above_height, below_height):
        if is_almost(point_y, height):
            return Point([self.get_x(), height])

    return self


def __adjust_elementary_path(self, milling_height):
    """
    slightly move endpoints in elementary path so that if they are very close
    from a milling height then they will be exactly at milling height.
    careful : we do not change arc's center point so this might lead
    to rounding errors.
    """
    self.endpoints = [p.adjust_at_milling_height(milling_height)
                      for p in self.endpoints]

setattr(Arc, "split_at_milling_points", __split_arcs)
setattr(Segment, "split_at_milling_points", __split_segments)
setattr(Point, "adjust_at_milling_height", __adjust_point)
setattr(ElementaryPath, "adjust_points_at_milling_height",
        __adjust_elementary_path)

from jimn.bounding_box import bounding_box
from jimn.displayable import tycat
from jimn.segment import segment
from jimn.utils.coordinates_hash import coordinates_hash
from jimn.utils.debug import is_module_debugged
from jimn.ghost import ghost
from jimn.graph import graph
from jimn.graph.even_degrees import make_degrees_even
from jimn.graph.internal_edges import create_internal_edges
from math import floor, ceil

"""
set of paths defining a pocket to mill.
"""


class pocket:
    def __init__(self, paths):
        self.paths = paths

    def get_points(self):
        for p in self.paths:
            yield p.get_endpoint(0)

    def milling_heights(self, milling_diameter):
        box = self.get_bounding_box()
        ymin, ymax = box.limits(1)
        start = floor(ymin / milling_diameter)
        end = ceil(ymax / milling_diameter)
        for i in range(start, end+1):
            yield i * milling_diameter

    def create_vertices(self, milling_diameter, built_graph):
        # first cut by horizontal lines spaced by milling_diameter
        box = self.get_bounding_box()
        xmin, xmax = box.limits(0)
        cutting_lines = [
            segment.horizontal_segment(xmin, xmax, y)
            for y in self.milling_heights(milling_diameter)
        ]
        # TODO is list needed ?
        cutter = ghost(list(self.paths))
        elementary_segments = cutter.compute_elementary_paths(
            cutting_lines
        ).get_content()
        # ok, now create graph, each segment point becomes a vertex
        # and we add all external edges
        for s in elementary_segments:
            built_graph.add_edge(s, frontier_edge=True)

    def round_points(self, rounder):
        for p in self.paths:
            p.round_points(rounder)

    def get_bounding_box(self):
        box = bounding_box.empty_box(2)
        for p in self.paths:
            box.update(p.get_bounding_box())
        return box

    def save_svg_content(self, display, color):
        for p in self.paths:
            p.save_svg_content(display, color)

    def build_graph(self, milling_diameter):
        """
        returns graph which will be used to compute milling path
        """
        if __debug__:
            if is_module_debugged(__name__):
                print("rounding points")
        # round all points on intersecting lines
        rounder = coordinates_hash(2)
        for y in self.milling_heights(milling_diameter):
            rounder.hash_coordinate(1, y)

        self.round_points(rounder)

        if __debug__:
            if is_module_debugged(__name__):
                print("creating vertices")
        # fill all vertices
        g = graph()
        self.create_vertices(milling_diameter, g)

        if __debug__:
            if is_module_debugged(__name__):
                tycat(g)
                print("creating internal edges")
        # finish by adding horizontal internal edges
        create_internal_edges(g, milling_diameter)
        if __debug__:
            if is_module_debugged(__name__):
                tycat(g)
                print("making degrees even")

        # prepare for eulerian path
        make_degrees_even(g)
        if __debug__:
            if is_module_debugged(__name__):
                tycat(g)
                print("done")

        return g

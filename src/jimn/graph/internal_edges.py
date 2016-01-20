"""
fill pocket with internal paths.
"""
from collections import defaultdict
from jimn.displayable import tycat
from jimn.graph.edge import Edge
from jimn.utils.math import is_slice_height
from jimn.segment import Segment
from jimn.utils.debug import is_module_debugged


def create_internal_edges(graph, milling_diameter):
    """
    create internal horizontal edges for milling.
    """
    vertices_per_height = defaultdict(list)
    for vertex in graph.vertices:
        vertex_y = vertex.get_y()
        if is_slice_height(vertex_y, milling_diameter):
            vertices_per_height[vertex_y].append(vertex)

    for milling_y, vertices in vertices_per_height.items():
        _create_internal_edges_in_slice(graph, milling_y, sorted(vertices))


def _create_internal_edges_in_slice(graph, milling_y, vertices):
    """
    move on slice line. when inside add edge.
    """
    current_position = Position(milling_y, outside=True)
    for edge in _horizontal_edges(vertices):
        current_position.update(edge)
        if current_position.is_inside():
            graph.add_direct_edge(edge)
            if __debug__:
                if is_module_debugged(__name__):
                    print("adding horizontal edge", str(current_position))
                    tycat(graph, edge)
        else:
            if __debug__:
                if is_module_debugged(__name__):
                    print("not adding horizontal edge", str(current_position))
                    tycat(graph, edge)


class Position:
    """
    small class to remember where we are while following a horizontal line
    crossing a pocket.
    """
    def __init__(self, milling_y, outside):
        self.outside = outside
        self.on_edge = False
        self.on_edge_inside_is_above = None
        self.milling_y = milling_y

    def __str__(self):
        return "out:{} on_edge:{} on_edge_inside_above:{}".format(
            self.outside, self.on_edge, self.on_edge_inside_is_above
        )

    def update(self, edge):
        """
        move through given edge and update position.
        """
        start_vertex = edge.vertices[0]
        # many cases here
        # we look at edges starting from start_vertex
        # to figure out current position
        if not self.on_edge:
            if not start_vertex.has_frontier_edge(edge):
                # easy case : we were not on edge
                # and are not on edge
                # edges on different sides of y flip position
                if start_vertex.has_frontier_edges_on_different_sides_of(
                        self.milling_y):
                    self.outside = not self.outside
            else:
                # harder case, we are now on edge of polygon
                self.on_edge = True
                other_edge = start_vertex.other_frontier_edge(edge)
                # remember where is the inside with respect to us
                if other_edge.is_above_y(self.milling_y):
                    self.on_edge_inside_is_above = self.outside
                else:
                    self.on_edge_inside_is_above = not self.outside
        else:
            # hardest case
            # we were on edge of polygon and are leaving it
            # so we are back outside or inside
            self.on_edge = False
            non_horizontal_edge = \
                start_vertex.get_non_horizontal_frontier_edge()
            if non_horizontal_edge.is_above_y(self.milling_y):
                #               /
                #      inside  / outside
                #             /
                # ------------***edge***
                self.outside = self.on_edge_inside_is_above
            else:
                self.outside = not self.on_edge_inside_is_above

    def is_inside(self):
        """
        are we inside pocket ?
        """
        if self.on_edge:
            return False
        return not self.outside


def _horizontal_edges(aligned_vertices):
    """
    iterate through all horizontal edges (containing segments)
    between given horizontally aligned vertices.
    """
    for index in range(len(aligned_vertices)-1):
        vertices = [aligned_vertices[index],
                    aligned_vertices[(index+1) % len(aligned_vertices)]]
        objects = [v.bound_object for v in vertices]
        yield Edge(*vertices, Segment(objects))

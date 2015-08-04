from jimn.displayable import tycat
from jimn.graph.edge import edge
from jimn.point import is_slice_height
from jimn.segment import segment
from jimn.utils.debug import is_module_debugged
from collections import defaultdict


def create_internal_edges(g, milling_diameter):
    """
    create internal horizontal edges for milling
    """
    vertices_per_height = defaultdict(list)
    for v in g.get_vertices():
        y = v.get_y()
        if is_slice_height(y, milling_diameter):
            vertices_per_height[y].append(v)

    for y, vertices_y in vertices_per_height.items():
        _create_internal_edges_in_slice(g, y, sorted(vertices_y))


def _create_internal_edges_in_slice(g, y, vertices):
    p = position(y, g, outside=True)
    for e in _horizontal_edges(vertices):
        p.update(e)
        if p.is_inside():
            g.add_direct_edge(e)
            if __debug__:
                if is_module_debugged(__name__):
                    print("adding horizontal edge", str(p))
                    tycat(g, e)
        else:
            if __debug__:
                if is_module_debugged(__name__):
                    print("not adding horizontal edge", str(p))
                    tycat(g, e)


class position:
    def __init__(self, y, g, outside):
        self.outside = outside
        self.on_edge = False
        self.on_edge_inside_is_above = None
        self.graph = g
        self.y = y

    def __str__(self):
        return "out:{} on_edge:{} on_edge_inside_above:{}".format(
            self.outside, self.on_edge, self.on_edge_inside_is_above
        )

    def update(self, e):
        start_vertex = e.get_endpoint(0)
        # many cases here
        # we look at edges starting from start_vertex
        # to figure out current position
        if not self.on_edge:
            if not start_vertex.has_frontier_edge(e):
                # easy case : we were not on edge
                # and are not on edge
                # edges on different sides of y flip position
                if start_vertex.has_frontier_edges_on_different_sides_of(
                    self.y
                ):
                    self.outside = not self.outside
            else:
                # harder case, we are now on edge of polygon
                self.on_edge = True
                other_edge = start_vertex.other_frontier_edge(e)
                # remember where is the inside with respect to us
                if other_edge.is_above_y(self.y):
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
            if non_horizontal_edge.is_above_y(self.y):
                #               /
                #      inside  / outside
                #             /
                # ------------***edge***
                self.outside = self.on_edge_inside_is_above
            else:
                self.outside = not self.on_edge_inside_is_above

    def is_inside(self):
        if self.on_edge:
            return False
        return not self.outside


def _horizontal_edges(aligned_vertices):
    """iterates through all horizontal segments
    between given horizontally aligned vertices"""
    for i in range(len(aligned_vertices)-1):
        v1 = aligned_vertices[i]
        v2 = aligned_vertices[(i+1) % len(aligned_vertices)]
        p1 = v1.to_point()
        p2 = v2.to_point()
        yield edge(v1, v2, segment([p1, p2]))

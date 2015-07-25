from jimn.segment import segment
from jimn.vertex import vertex
from jimn.point import is_slice_height
from jimn.bounding_box import bounding_box
from jimn.path import path
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from collections import defaultdict


class graph:
    def __init__(self):
        self.vertices = {}

    def is_empty(self):
        return len(self.vertices) == 0

    def get_vertices(self):
        return self.vertices.values()

    def get_any_vertex(self):
        """
        return a vertex
        """
        return next(iter(self.vertices.values()))

    def get_vertex(self, vertex_point):
        if vertex_point in self.vertices:
            return self.vertices[vertex_point]
        else:
            return None

    def get_bounding_box(self):
        box = bounding_box.empty_box(2)
        for p in self.get_vertices():
            box.add_point(p)
        return box

    def save_svg_content(self, display, color):
        for p in self.get_vertices():
            p.save_svg_content(display, color)

    def add_vertex(self, vertex_point):
        if vertex_point not in self.vertices:
            self.vertices[vertex_point] = vertex(vertex_point)
        return self.vertices[vertex_point]

    def add_edge(self, edge_path):
        endpoints = edge_path.get_endpoints()
        assert endpoints[0] in self.vertices, "no such vertex"
        assert endpoints[1] in self.vertices, "no such vertex"
        self.vertices[endpoints[0]].add_edge(edge_path)
        self.vertices[endpoints[1]].add_edge(edge_path.reverse())

    def create_internal_edges(self, milling_diameter):
        vertices_per_height = defaultdict(list)
        for v in self.vertices.values():
            y = v.get_y()
            if is_slice_height(y, milling_diameter):
                vertices_per_height[y].append(v)

        for y, vertices_y in vertices_per_height.items():
            self._create_internal_edges_in_slice(y, sorted(vertices_y))

    def make_degrees_even(self):
        for v in self.vertices.values():
            if not v.even_degree():
                self._create_edge_from_vertex(v)

    def find_eulerian_cycle(self):
        cycle_starts = defaultdict(list)  # where do found cycles start
        possible_starts = {}  # where to search for a new cycle
        start_vertex = self.get_any_vertex()
        possible_starts[start_vertex] = start_vertex.degree()
        first_cycle = None
        while not self.is_empty():
            c = self._find_cycle(possible_starts)
            tycat(self, start_vertex, c)
            if first_cycle is None:
                first_cycle = c
            else:
                cycle_start = self.vertices[c.get_start()]
                cycle_starts[cycle_start].append(c)
        first_cycle.fuse_with(cycle_starts)
        return first_cycle

    def _find_cycle(self, possible_starts):
        start_vertex = next(iter(possible_starts.keys()))
        current_vertex = start_vertex
        edges = []
        while True:
            current_edge = current_vertex.remove_any_edge()
            self._update_possible_starts(possible_starts, current_vertex)
            next_point = current_edge.get_endpoint(1)
            edges.append(current_edge)
            if next_point not in self.vertices:
                break
            next_vertex = self.vertices[next_point]
            next_vertex.remove_edge_to(current_vertex)
            self._update_possible_starts(possible_starts, next_vertex)
            current_vertex = next_vertex
            tycat(self, edges)
        return path(edges)

    def _update_possible_starts(self, possible_starts, decreased_vertex):
        degree = decreased_vertex.degree()
        if degree:
            possible_starts[decreased_vertex] = degree
        else:
            del possible_starts[decreased_vertex]
            del self.vertices[decreased_vertex]

    # requires that in each vertex, the first two edges are border edges
    # otherwise, we may add internal edges too
    def _create_edge_from_vertex(self, v):

        previous_vertex = None
        current_vertex = v
        while not current_vertex.even_degree():
            edge = current_vertex.find_first_neighbor_not(previous_vertex)
            self.add_edge(edge)
            next_point = edge.get_endpoint(1)  # edge goes from current to next
            next_vertex = self.vertices[next_point]
            previous_vertex = current_vertex
            current_vertex = next_vertex

    def _create_internal_edges_in_slice(self, y, vertices):
        p = position(y, self, outside=True)
        for e in _horizontal_edges(vertices):
            p.update(e)
            if p.is_inside():
                self.add_edge(e)
                if __debug__:
                    if is_module_debugged(__name__):
                        print("adding horizontal edge", str(p))
                        tycat(self, e)
            else:
                if __debug__:
                    if is_module_debugged(__name__):
                        print("not adding horizontal edge", str(p))
                        tycat(self, e)


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

    def update(self, edge):
        start_point = edge.get_endpoint(0)
        start_vertex = self.graph.get_vertex(start_point)
        # many cases here
        # we look at edges starting from start_vertex
        # to figure out current position
        if not self.on_edge:
            if not start_vertex.has_edge(edge):
                # easy case : we were not on edge
                # and are not on edge
                # edges on different sides of y flip position
                if start_vertex.has_edges_on_different_sides_of(self.y):
                    self.outside = not self.outside
            else:
                # harder case, we are now on edge of polygon
                self.on_edge = True
                other_edge = start_vertex.other_edge(edge)
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
            non_horizontal_edge = start_vertex.get_non_horizontal_edge()
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
        yield segment([v1, v2])

from jimn.algorithms.bellman_ford import bellman_ford
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

    def get_vertices_number(self):
        return len(self.vertices)

    def get_edges_from(self, start):
        return self.vertices[start].get_edges()

    def get_all_edges(self):
        for v in self.vertices.values():
            for e in v.get_edges():
                yield e

    def get_edge_weight(self, start_vertex, edge):
        return self.vertices[start_vertex].get_edge_weight(edge)

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

    def add_edge(self, edge):
        endpoints = edge.get_endpoints()
        assert endpoints[0] in self.vertices, "no such vertex"
        assert endpoints[1] in self.vertices, "no such vertex"
        self.vertices[endpoints[0]].add_edge(edge)
        self.vertices[endpoints[1]].add_edge(edge.reverse())

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
                self._augment_path(v)

    def find_eulerian_cycle(self):
        """
        eulerian cycle classical algorithm.
        requires all degrees to be even.
        """
        # we loop finding cycles until graph is empty
        possible_starts = {}  # where to search for a new cycle
        start_vertex = self.get_any_vertex()
        # we constrain possible starting points
        # to be only a previous cycles
        # this will enable easier merging of all cycles
        possible_starts[start_vertex] = start_vertex.degree()
        # we just need to remember for each cycle its starting point
        cycle_starts = defaultdict(list)  # where do found cycles start

        first_cycle = None
        while not self.is_empty():
            c = self._find_cycle(possible_starts)
            if __debug__:
                if is_module_debugged(__name__):
                    print("found new cycle")
                    tycat(self, c)
            if first_cycle is None:
                first_cycle = c
            else:
                cycle_start = c.get_start()
                cycle_starts[cycle_start].append(c)
        first_cycle.fuse_with(cycle_starts)
        return first_cycle

    def _find_cycle(self, possible_starts):
        start_vertex = next(iter(possible_starts.keys()))
        current_vertex = start_vertex
        edges = []
        while current_vertex.degree() != 0:

            current_edge = current_vertex.remove_any_edge()
            self._update_possible_starts(possible_starts, current_vertex)
            edges.append(current_edge)

            next_point = current_edge.get_endpoint(1)
            next_vertex = self.vertices[next_point]
            next_vertex.remove_edge_to(current_vertex)
            self._update_possible_starts(possible_starts, next_vertex)
            current_vertex = next_vertex

        return path(edges)

    def _update_possible_starts(self, possible_starts, decreased_vertex):
        degree = decreased_vertex.degree()
        if degree:
            possible_starts[decreased_vertex] = degree
        else:
            del possible_starts[decreased_vertex]
            del self.vertices[decreased_vertex]

    def _augment_path(self, v):
        distances, predecessors = bellman_ford(self, v)
        destination = self._find_nearest_odd_vertex(v, distances)
        print("going to", destination)
        current_point = destination
        if __debug__:
            if is_module_debugged(__name__):
                added_edges = []
        while current_point != v:
            edge = predecessors[current_point]
            self.add_edge(edge)
            if __debug__:
                if is_module_debugged(__name__):
                    added_edges.append(edge)
            previous_point = edge.get_endpoint(0)
            current_point = previous_point
            tycat(self, v, destination, added_edges, current_point)
        if __debug__:
            if is_module_debugged(__name__):
                print("new augmenting path")
                tycat(self, added_edges)

    def _find_nearest_odd_vertex(self, v, distances):
        current_distance = float("inf")
        for destination, distance in distances.items():
            if (destination != v) and (current_distance > distance):
                destination = self.vertices[destination]
                if destination.degree() % 2:
                    best_destination = destination
        return best_destination

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
            if not start_vertex.has_initial_edge(edge):
                # easy case : we were not on edge
                # and are not on edge
                # edges on different sides of y flip position
                if start_vertex.has_initial_edges_on_different_sides_of(self.y):
                    self.outside = not self.outside
            else:
                # harder case, we are now on edge of polygon
                self.on_edge = True
                other_edge = start_vertex.other_initial_edge(edge)
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

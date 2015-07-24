from jimn.segment import segment
from jimn.vertex import vertex
from jimn.point import is_slice_height
from jimn.bounding_box import bounding_box
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from collections import defaultdict
from queue import Queue
import pdb


class graph:
    def __init__(self):
        self.vertices = {}

    def get_vertices(self):
        return self.vertices.values()

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

    def find_eulerian_path(self):
        cycles, cycles_by_start_points = self.get_cycles()
        c1 = cycles[0]
        res = []
        while c1 != []:
            e = c1.pop()
            res.append(e)
            p = e.get_endpoint(1)
            if p in cycles_by_start_points:
                c = cycles_by_start_points[p].pop()
                c1.extend(c)

    def get_cycles(self):
        pdb.set_trace()

        cycles = []
        cycles_by_start_points = defaultdict(list)
        potential_starting_points = Queue()
        # on ajoutera start_point dans Queue pour généraliser le code

        cycle = []

        start_point, start_vertex = list(self.vertices.items())[0]
        cycles_by_start_points[start_vertex].append(cycle)
        potential_starting_points.put(start_vertex)

        while not potential_starting_points.empty():
            start_vertex = potential_starting_points.get()

            if start_vertex.degree() == 0:
                continue

            previous_vertex = start_vertex
            edge = previous_vertex.get_edge(0)
            current_point = edge.get_endpoint(1)
            current_vertex = self.vertices[current_point]
            cycle.append(edge)
            # del self.vertices[current_point]

            # previous_vertex.delete_edge(edge)
            current_vertex.delete_edge(edge)
            if previous_vertex.degree() > 1:
                potential_starting_points.put(previous_vertex)
            if current_vertex.degree() > 1:
                potential_starting_points.put(current_vertex)

            while current_vertex != start_vertex:
                # tycat(self, edge, cycle)
                previous_vertex = current_vertex
                previous_edge = edge
                edge = current_vertex.other_edge(previous_edge)
                cycle.append(edge)
                current_point = edge.get_endpoint(1)
                if current_point == start_point:
                    current_vertex = start_vertex
                else:
                    current_vertex = self.vertices[current_point]
                    # del self.vertices[current_point]

                # previous_vertex.delete_edge(edge)
                tycat(self, edge)
                current_vertex.delete_edge(edge)
                if current_vertex.degree() > 1:
                    potential_starting_points.put(current_vertex)

            cycles.append(cycle)

        return cycles, cycles_by_start_points

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

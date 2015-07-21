from jimn.segment import segment
from jimn.segment import are_traversing
from jimn.vertex import vertex
from jimn.point import is_slice_height
from jimn.elementary_path import same_paths
from collections import defaultdict


class graph:
    def __init__(self):
        self.vertices = {}

    def get_vertices(self):
        return self.vertices.values()

    def add_vertex(self, vertex_point):
        if vertex_point not in self.vertices:
            self.vertices[vertex_point] = vertex(vertex_point)
        return self.vertices[vertex_point]

    def create_internal_edges(self, milling_diameter):
        vertices_per_height = defaultdict(list)
        for v in self.vertices.values():
            vertices_per_height[v.get_y()].append(v)

        for y, vertices_y in vertices_per_height.items():
            if is_slice_height(y, milling_diameter):
                create_internal_edges_in_slice(vertices_y)

    def make_degrees_even(self):
        for v in self.vertices.values():
            if not v.even_degree():
                self.create_edge_from_vertex(v)

    # requires that in each vertex, the first two edges are border edges
    # otherwise, we may add internal edges too
    def create_edge_from_vertex(self, v):
        complex_edge = []

        starting_path = v.get_edge(0)
        complex_edge.append(starting_path)
        old_vertex = v
        old_path = starting_path
        new_vertex = self.vertices[old_path.get_endpoint(1)]
        while new_vertex.even_degree():
            e1, e2 = new_vertex.get_edges()[:2]
            if same_paths(e1, old_path):
                new_path = e2
            else:
                assert same_paths(e2, old_path)
                new_path = e1
            complex_edge.append(new_path)
            old_vertex = new_vertex
            old_path = new_path
            new_vertex = self.vertices[old_path.get_endpoint(1)]

        v.add_edge(complex_edge)
        reversed_complex_edge = [e.reverse() for e in reversed(complex_edge)]
        new_vertex.add_edge(reversed_complex_edge)


class state:
    def __init__(self, inside, v=None):
        self.inside = inside
        self.v = v
        self.starts_horizontal_path = False
        self.non_horizontal_path = None

    def is_inside(self):
        return self.inside

    def get_vertex(self):
        return self.v

    def change(self):
        self.inside = not self.inside

    def mark_starting_horizontal_path(self):
        self.starts_horizontal_path = True
        self.non_horizontal_path = self.v.get_non_horizontal_path()

    def starting_horizontal_path(self):
        return self.starts_horizontal_path

    def get_non_horizontal_path(self):
        return self.non_horizontal_path


def create_internal_edges_in_slice(vertices):
    new_edges = []
    vertices = sorted(vertices)
    old_state = state(inside=False)
    add_edge = False

    for v in vertices:
        if add_edge:
            new_edges.append(segment([old_state.get_vertex(), v]))

        new_state = state(old_state.is_inside(), v)

        if v.has_horizontal_path():
            if old_state.starting_horizontal_path():
                s1 = old_state.get_non_horizontal_path()
                s2 = v.get_non_horizontal_path()
                crossing_border = are_traversing(s1, s2.reverse())
            else:
                crossing_border = False
                new_state.mark_starting_horizontal_path()
        else:
            crossing_border = v.is_traversed_by_paths()

        if crossing_border:
            new_state.change()

        add_edge = new_state.is_inside() and not new_state.starting_horizontal_path()
        old_state = new_state

    create_edges_from_paths(new_edges)


def create_edges_from_paths(paths):
    for p in paths:
        v1, v2 = p.get_endpoints()
        v1.add_edge(p)
        v2.add_edge(p.reverse())

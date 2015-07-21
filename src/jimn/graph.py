from jimn.segment import segment
from jimn.segment import are_traversing
from jimn.vertex import vertex
from jimn.point import is_slice_height
from collections import defaultdict
from jimn.bounding_box import bounding_box


class graph:
    def __init__(self):
        self.vertices = {}

    def get_vertices(self):
        return self.vertices.values()

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

    def create_internal_edges(self, milling_diameter):
        vertices_per_height = defaultdict(list)
        for v in self.vertices.values():
            y = v.get_y()
            if is_slice_height(y, milling_diameter):
                vertices_per_height[y].append(v)

        for y, vertices_y in vertices_per_height.items():
            create_internal_edges_in_slice(vertices_y)

    def make_degrees_even(self):
        for v in self.vertices.values():
            if not v.even_degree():
                self.create_edge_from_vertex(v)

    # requires that in each vertex, the first two edges are border edges
    # otherwise, we may add internal edges too
    def create_edge_from_vertex(self, v):

        previous_vertex = None
        current_vertex = v
        while not current_vertex.even_degree():
            edge = current_vertex.find_first_neighbor_not(previous_vertex)
            next_point = edge.get_endpoint(1)  # edge goes from current to next
            next_vertex = self.vertices[next_point]
            current_vertex.add_edge(edge)
            next_vertex.add_edge(edge.reverse())
            previous_vertex = current_vertex
            current_vertex = next_vertex


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

        add_edge = (new_state.is_inside() and
                    not new_state.starting_horizontal_path())
        old_state = new_state

    create_edges_from_paths(new_edges)


def create_edges_from_paths(paths):
    for p in paths:
        v1, v2 = p.get_endpoints()
        v1.add_edge(p)
        v2.add_edge(p.reverse())

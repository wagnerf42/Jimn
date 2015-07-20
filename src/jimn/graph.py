from jimn.segment import segment
from jimn.segment import are_traversing
from jimn.vertex import vertex
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

        for vertices_y in vertices_per_height.values():
            create_internal_edges_in_slice(vertices_y)


class state:
    def __init__(self, inside):
        self.inside = inside
        self.has_horizontal_path = False
        self.non_horizontal_path = None

    def is_inside(self):
        return self.inside

    def change(self):
        self.inside = not self.inside


def create_internal_edges_in_slice(vertices):
    new_edges = []
    vertices = sorted(vertices)
    prec_v = None   # useful ?
    prec_state = state(inside=False)
    add_edge = False

    for v in vertices:
        crossing_border = False
        if add_edge:
            new_edges.append(segment([prec_v, v]))

        new_state = state(inside=prec_state.is_inside())
        if v.has_horizontal_path():
            if prec_state.has_horizontal_path:
                s1 = prec_state.non_horizontal_path
                s2 = v.get_non_horizontal_path()
                crossing_border = are_traversing(s1, s2.reverse())
            else:
                # mark state as having beginning horizontal path
                new_state.has_horizontal_path = True
                new_state.non_horizontal_path = v.get_non_horizontal_path()
        else:
            crossing_border = v.is_traversed_by_paths()

        if crossing_border:
            new_state.change()
        add_edge = new_state.is_inside() and not new_state.has_horizontal_path
        prec_state = new_state
        prec_v = v

    for e in new_edges:
        p1, p2 = e.get_endpoints()
        p1.add_edge(e)
        p2.add_edge(e.reverse())

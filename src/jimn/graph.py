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

def create_internal_edges_in_slice(vertices):
    new_edges = []
    vertices = sorted(vertices)
    prec_v = None   # useful ?
    prec_state = state(inside=False)
    adding = False

    for v in vertices:
        if adding:
            new_edges.append(segment([prec_v, v]))
            adding = False

        if v.has_horizontal_path():
            if prec_state.has_horizontal_path:
                s1 = prec_state.non_horizontal_path
                s2 = v.get_non_horizontal_path()
                if are_traversing(s1, s2.reverse()):
                    new_state = state(inside=not prec_state.is_inside())
                else:
                    new_state = state(inside=prec_state.is_inside())
                if new_state.inside:
                    adding = True
            else:
                new_state = state(inside=prec_state.is_inside())
                # mark state as having beginning horizontal path
                new_state.has_horizontal_path = True
                new_state.non_horizontal_path = v.get_non_horizontal_path()
        elif v.is_traversed_by_paths():
            new_state = state(inside=not prec_state.is_inside())
            if new_state.is_inside():
                adding = True
        else:
            new_state = state(inside=prec_state.is_inside())
            if new_state.is_inside():
                adding = True
        prec_state = new_state
        prec_v = v

    for e in new_edges:
        p1, p2 = e.get_endpoints()
        p1.add_edge(e)
        p2.add_edge(e.reverse())

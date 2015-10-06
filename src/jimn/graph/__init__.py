from jimn.graph.vertex import vertex
from jimn.graph.edge import edge
from jimn.bounding_box import bounding_box


class graph:
    def __init__(self):
        self.vertices_objects = {}
        self.vertices = []
        self.vertices_number = 0
        self.max_vertices_number = 0

    def is_empty(self):
        return self.vertices_number == 0

    def get_vertices(self):
        return self.vertices

    def get_vertices_number(self):
        return self.vertices_number

    def get_max_vertices_number(self):
        return self.max_vertices_number

    def get_all_edges(self):
        for v in self.vertices:
            for e in v.get_edges():
                yield e

    def get_any_vertex(self):
        """
        return a vertex
        """
        return self.vertices[0]

    def get_bounding_box(self):
        box = bounding_box.empty_box(2)
        for p in self.get_vertices():
            box.add_point(p.bound_object)
        return box

    def save_svg_content(self, display, color):
        for p in self.get_vertices():
            p.save_svg_content(display, color)

    def add_vertex(self, vertex_object):
        """
        adds a vertex corresponding to given object.
        if one is already in graph then does not add but returns existing one
        """
        if vertex_object not in self.vertices_objects:
            v = vertex(vertex_object, self.max_vertices_number)
            self.vertices_objects[vertex_object] = v
            self.vertices.append(v)
            self.max_vertices_number += 1
            self.vertices_number += 1
        return self.vertices_objects[vertex_object]

    def remove_vertex(self, v):
        self.vertices.remove(v)
        self.vertices_number -= 1

    def add_edge(self, edge_path, frontier_edge=False):
        endpoints = edge_path.get_endpoints()
        vertices = [self.add_vertex(p) for p in endpoints]
        e = edge(vertices[0], vertices[1], edge_path)
        vertices[0].add_edge(e, frontier_edge)
        reversed_e = edge(vertices[1], vertices[0], edge_path.reverse())
        vertices[1].add_edge(reversed_e, frontier_edge)

    def add_direct_edge(self, e):
        """
        adds an edge (non frontier)
        between two existing vertices
        """
        vertices = e.get_endpoints()
        vertices[0].add_edge(e, frontier_edge=False)
        tmp = e.reverse()
        vertices[1].add_edge(tmp, frontier_edge=False)

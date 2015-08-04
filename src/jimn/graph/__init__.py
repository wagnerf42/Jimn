from jimn.graph.vertex import vertex
from jimn.graph.edge import edge
from jimn.bounding_box import bounding_box


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

    def get_any_vertex(self):
        """
        return a vertex
        """
        return next(iter(self.vertices.values()))

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

    def remove_vertex(self, v):
        del self.vertices[v]

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
        vertices[1].add_edge(e.reverse(), frontier_edge=False)

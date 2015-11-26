

class graph:
    def __init__(self):
        self.vertices_objects = {}
        self.vertices = []
        self.vertices_number = 0
        self.max_vertices_number = 0

    @classmethod
    def complete_graph(cls, points):
        """
        builds complete graph from set of points.
        """
        g = cls()
        for p1, p2 in combinations(points, 2):
            g.add_edge(segment([p1, p2]))
        return g

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

    def get_non_oriented_edges(self):
        """
        iterates through edges.
        in graph : existence of edge (a,b) implies existence of (b,a)
        we only iterate here once on each edge (avoiding reversed edges)
        """
        seen_vertices = {}
        for v in self.vertices:
            seen_vertices[v] = True
            for e in v.get_edges():
                if e.vertices[1] not in seen_vertices:
                    yield e

    def get_any_vertex(self):
        """
        return a vertex
        """
        return self.vertices[0]

    def get_bounding_box(self):
        box = bounding_box.empty_box(2)
        for p in self.get_vertices():
            small_box = p.get_object().get_bounding_box()
            box.update(small_box)
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
        self.add_edge_between(endpoints[0], endpoints[1],
                              edge_path, frontier_edge)

    def add_edge_between(self, object1, object2, edge_path,
                         frontier_edge=False):
        """
        creates or get vertices corresponding to given objects and add an edge
        between them with the given path.
        """
        vertex1 = self.add_vertex(object1)
        vertex2 = self.add_vertex(object2)
        e = edge(vertex1, vertex2, edge_path)
        vertex1.add_edge(e, frontier_edge)
        reversed_e = edge(vertex2, vertex1, edge_path.reverse())
        vertex2.add_edge(reversed_e, frontier_edge)

    def add_direct_edge(self, e):
        """
        adds an edge (non frontier)
        between two existing vertices
        """
        vertices = e.get_endpoints()
        vertices[0].add_edge(e, frontier_edge=False)
        tmp = e.reverse()
        vertices[1].add_edge(tmp, frontier_edge=False)

    def subgraph(self, vertices):
        """
        returns subgraph formed by given vertices.
        """
        subg = graph()
        for v1, v2 in combinations(vertices, 2):
            e = v1.get_edge_to(v2)
            p = e.get_path()
            subg.add_edge_between(v1.get_object(), v2.get_object(), p)
        return subg

    def get_double_edges(self):
        """
        returns list of our edges with multiplicity 2.
        """
        double_edges = []
        for e in self.get_non_oriented_edges():
            if e.get_multiplicity() == 2:
                double_edges.append(e)
        return double_edges

from jimn.bounding_box import bounding_box
from jimn.graph.edge import edge
from jimn.graph.vertex import vertex
from jimn.segment import segment
from itertools import combinations

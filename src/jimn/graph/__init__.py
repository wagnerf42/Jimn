"""
graph class and algorithms.
"""
from itertools import combinations
from jimn.bounding_box import Bounding_Box
from jimn.graph.edge import Edge
from jimn.graph.vertex import Vertex
from jimn.segment import Segment


class Graph:
    """
    oriented multigraph.
    two types of edges (frontier and standard).
    """
    def __init__(self):
        self.vertices_objects = {}
        self.vertices = []
        self.vertices_number = 0
        self.max_vertices_number = 0

    def frontier_edges(self):
        """
        iterator going through all frontier edges.
        we only do non-oriented style : avoid seeing edge once in each direction
        """
        for vertex in self.vertices:
            for edge in vertex.frontier_edges:
                if edge.vertices[0].unique_id < edge.vertices[1].unique_id:
                    yield edge

    @classmethod
    def complete_graph(cls, points):
        """
        builds complete graph from set of points.
        """
        graph = cls()
        for points in combinations(points, 2):
            graph.add_edge(Segment(points))
        return graph

    def is_empty(self):
        """
        do we contain nothing ?
        """
        return self.vertices_number == 0

    def get_max_vertices_number(self):
        """
        return upper bound (strict) on max vertex id.
        """
        return self.max_vertices_number

    def get_all_edges(self):
        """
        iterator on edges.
        """
        for vertex in self.vertices:
            for edge in vertex.edges:
                yield edge

    def get_non_oriented_edges(self):
        """
        iterator on edges.
        in graph : existence of edge (a,b) implies existence of (b,a)
        we only iterate here once on each edge (avoiding reversed edges)
        """
        for vertex in self.vertices:
            for edge in vertex.edges:
                if edge.vertices[0].unique_id < edge.vertices[1].unique_id:
                    yield edge

    def get_any_vertex(self):
        """
        return a vertex
        """
        return self.vertices[0]

    def get_bounding_box(self):
        """
        min bounding box containing vertices objects.
        """
        box = Bounding_Box.empty_box(2)
        for vertex in self.vertices:
            small_box = vertex.bound_object.get_bounding_box()
            box.update(small_box)
        return box

    def save_svg_content(self, display, color):
        """
        svg for tycat.
        """
        for vertex in self.vertices:
            vertex.save_svg_content(display, color)

    def add_vertex(self, vertex_object):
        """
        add a vertex corresponding to given object.
        if one is already in graph then do not add but return existing one.
        """
        if vertex_object not in self.vertices_objects:
            vertex = Vertex(vertex_object, self.max_vertices_number)
            self.vertices_objects[vertex_object] = vertex
            self.vertices.append(vertex)
            self.max_vertices_number += 1
            self.vertices_number += 1

        return self.vertices_objects[vertex_object]

    def remove_vertex(self, vertex):
        """
        remove one vertex. (leaves holes in vertices ids range).
        """
        self.vertices.remove(vertex)
        self.vertices_number -= 1

    def add_edge(self, edge_path, frontier_edge=False):
        """
        add given path as edge.
        create vertices if needed.
        """
        endpoints = edge_path.get_endpoints()
        self.add_edge_between(endpoints[0], endpoints[1],
                              edge_path, frontier_edge)

    def add_edge_between(self, object1, object2, edge_path,
                         frontier_edge=False):
        """
        create or get vertices corresponding to given objects and add an edge
        between them with the given path.
        """
        vertex1 = self.add_vertex(object1)
        vertex2 = self.add_vertex(object2)
        edge = Edge(vertex1, vertex2, edge_path)
        vertex1.add_edge(edge, frontier_edge)
        reversed_edge = Edge(vertex2, vertex1, edge_path.reverse())
        vertex2.add_edge(reversed_edge, frontier_edge)

    @classmethod
    def subgraph(cls, vertices):
        """
        return subgraph formed by given vertices.
        """
        subgraph = cls()
        for vertex1, vertex2 in combinations(vertices, 2):
            edge = vertex1.get_edge_to(vertex2)
            subgraph.add_edge_between(vertex1.bound_object,
                                      vertex2.bound_object, edge.path)
        return subgraph

    def double_edges(self):
        """
        iterate through edges with multiplicity 2.
        """
        for edge in self.get_non_oriented_edges():
            if edge.multiplicity == 2:
                yield edge

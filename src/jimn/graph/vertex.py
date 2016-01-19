"""
vertex in a graph (associated to geometric object).
"""


class Vertex:
    """
    a vertex stores an id, a list of all outgoing edges and a list
    of outgoing frontier edges, and an associated object.
    """
    def __init__(self, bound_object, number):
        self.bound_object = bound_object
        self.edges = []  # degree is very low so this is ok
        self.frontier_edges = []
        self.id = number

    def get_y(self):
        """
        used when bound object is point.
        """
        return self.bound_object.get_y()

    def get_edge_to(self, neighbor):
        """
        return edge to given neighbor.
        """
        for edge in self.edges:
            if edge.vertices[1] == neighbor:
                return edge
        raise Exception("no such neighbor")

    def get_bounding_box(self):
        """
        min bounding box for bound object and outgoing edges.
        """
        box = self.bound_object.get_bounding_box()
        for edge in self.edges:
            edge_box = edge.get_bounding_box()
            box.update(edge_box)
        return box

    def save_svg_content(self, display, color):
        """
        svg for tycat.
        """
        self.bound_object.save_svg_content(display, color)
        for edge in self.edges:
            path = edge.get_path()
            count = edge.get_multiplicity()
            path.save_svg_content(display, display.svg_color(count+7))

    def remove_any_edge(self):
        """
        remove and return one edge
        (decrease multiplicity in case of multi-edges).
        fast.
        """
        edge = self.edges[-1]
        if edge.get_multiplicity() == 1:
            self.edges.pop()
        else:
            edge.change_multiplicity(-1)
        return edge

    def remove_edge_to(self, destination):
        """
        remove one edge going to vertex destination
        (decrease multiplicity in case of multi-edges).
        slow.
        """
        for i, edge in enumerate(self.edges):
            if edge.vertices[1] == destination:
                self._delete_edge(i)
                return

        raise Exception("edge not found")

    def edge_index(self, searched_edge):
        """
        return index for given edge or None if not here.
        """
        for i, edge in enumerate(self.edges):
            if edge.is_same(searched_edge):
                return i
        return None

    def add_edge(self, edge, frontier_edge):
        """
        add given edge.
        if already here, increase edge multiplicity.
        """
        e_index = self.edge_index(edge)
        if e_index is not None:
            assert not frontier_edge, \
                "frontier edge {} added twice".format(str(edge.path))
            self.edges[e_index].change_multiplicity(1)
        else:
            self.edges.append(edge)

        if frontier_edge:
            self.frontier_edges.append(edge)

    def _delete_edge(self, i):
        multiplicity = self.edges[i].change_multiplicity(-1)
        if multiplicity == 0:
            del self.edges[i]

    def degree(self):
        """
        return degree (considering multi-edges).
        """
        return sum([e.get_multiplicity() for e in self.edges])

    def even_degree(self):
        """
        have we even degree ?
        """
        return self.degree() % 2 == 0

    def has_frontier_edge(self, searched_edge):
        """
        return true if one of our frontier edges
        is searched edge.
        """
        for edge in self.frontier_edges:
            if edge.is_same(searched_edge):
                return True

        return False

    def has_frontier_edges_on_different_sides_of(self, y_coordinate):
        """
        return true if our frontier edges
        are on different sides of horizontal line
        at given y
        """
        aboves = [e.is_above_y(y_coordinate) for e in self.frontier_edges]
        return aboves[0] != aboves[1]

    def other_frontier_edge(self, avoided_edge):
        """
        return edge in frontier edges
        which is not given edge.
        """
        for edge in self.frontier_edges:
            if not edge.is_same(avoided_edge):
                return edge

        raise Exception("no different edge")

    def get_non_horizontal_frontier_edge(self):
        """
        return edge in frontier edges
        which is not horizontal.
        """
        for edge in self.frontier_edges:
            if not edge.is_almost_horizontal():
                return edge

        raise Exception("only horizontal edges")

    def __eq__(self, other):
        """
        comparison of vertices is based on bound objects.
        """
        return id(self.bound_object) == id(other.bound_object)

    def __lt__(self, other):
        """
        comparison of vertices is based on bound objects.
        """
        return self.bound_object < other.bound_object

    def __hash__(self):
        """
        hashing of vertices is based on bound objects.
        """
        return hash(self.bound_object)

    def __str__(self):
        return str(id(self.bound_object)) + "\n\t" \
            + "\n\t".join([str(e) for e in self.edges])

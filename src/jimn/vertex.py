from jimn.point import point


class vertex(point):
    def __init__(self, position_point):
        super().__init__(position_point.get_coordinates())
        self.edges = {}
        self.initial_edges = []

    def get_edges(self):
        return self.edges.keys()

    def get_edge_weight(self, edge):
        if self.edges[edge] % 2:
            return edge.length()
        else:
            return -edge.length()

    def get_bounding_box(self):
        box = super(vertex, self).get_bounding_box()
        for e in self.edges.keys():
            edge_box = e.get_bounding_box()
            box.update(edge_box)
        return box

    def save_svg_content(self, display, color):
        super(vertex, self).save_svg_content(display, color)
        for e, count in self.edges.items():
            if e.is_sorted():
                e.save_svg_content(display, display.get_color(count+20))

    def remove_any_edge(self):
        e = next(iter(self.edges.keys()))
        self.delete_edge(e)
        return e

    def remove_edge_to(self, destination):
        """
        removes one edge going to destination
        """
        for edge in self.edges.keys():
            if edge.get_endpoint(1) == destination:
                self.delete_edge(edge)
                return
        else:
            raise Exception("edge not found")

    def add_edge(self, edge):
        if edge in self.edges:
            multiplicity = self.edges[edge]
        else:
            multiplicity = 0
        multiplicity = multiplicity + 1
        if multiplicity == 3:
            multiplicity = 1
        self.edges[edge] = multiplicity

    def add_initial_edge(self, edge):
        self.initial_edges.append(edge)
        self.add_edge(edge)

    def delete_edge(self, edge):
        multiplicity = self.edges[edge]
        multiplicity -= 1
        if multiplicity == 0:
            del self.edges[edge]
        else:
            self.edges[edge] = multiplicity

    def degree(self):
        return sum(self.edges.values())

    def even_degree(self):
        return self.degree() % 2 == 0

    def has_initial_edge(self, edge):
        """
        returns true if one of our intial edges
        is given edge
        """
        for e in self.initial_edges:
            if e.is_same(edge):
                return True
        return False

    def has_initial_edges_on_different_sides_of(self, y):
        """
        returns true if our initial edges
        are on different sides of horizontal line
        at given y
        """
        aboves = [e.is_above_y(y) for e in self.initial_edges]
        return (aboves[0] != aboves[1])

    def other_initial_edge(self, edge):
        """
        returns edge in initial_edges
        which is not given edge
        """
        for e in self.initial_edges:
            if not e.is_same(edge):
                return e
        raise Exception("no different edge")

    def get_non_horizontal_initial_edge(self):
        """
        returns edge in initial_edges
        which is not horizontal
        """
        for e in self.initial_edges:
            if not e.is_horizontal():
                return e
        raise Exception("only horizontal edges")

    def find_first_neighbor_not(self, neighbor):
        for e in self.edges.keys():
            if neighbor is None or e.get_endpoint(1) != neighbor:
                return e
        raise Exception("only one neighbor")

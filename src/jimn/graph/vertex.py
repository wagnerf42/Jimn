from jimn.point import point


class vertex(point):
    def __init__(self, position_point):
        super().__init__(position_point.get_coordinates())
        self.edges = {}
        self.frontier_edges = []

    def get_edges(self):
        return self.edges.keys()

    def get_edge_multiplicity(self, e):
        return self.edges[e]

    def get_bounding_box(self):
        box = super(vertex, self).get_bounding_box()
        for e in self.edges.keys():
            edge_box = e.get_bounding_box()
            box.update(edge_box)
        return box

    def save_svg_content(self, display, color):
        super(vertex, self).save_svg_content(display, color)
        for e, count in self.edges.items():
            p = e.get_path()
            if p.is_sorted():
                p.save_svg_content(display, display.get_color(count+20))

    def remove_any_edge(self):
        e = next(iter(self.edges.keys()))
        self.delete_edge(e)
        return e

    def remove_edge_to(self, destination):
        """
        removes one edge going to destination
        """
        for e in self.edges.keys():
            if e.get_endpoint(1) == destination:
                self.delete_edge(e)
                return
        else:
            raise Exception("edge not found")

    def add_edge(self, e, frontier_edge):
        if e in self.edges:
            multiplicity = self.edges[e]
        else:
            multiplicity = 0
        multiplicity = multiplicity + 1
        if multiplicity == 3:
            multiplicity = 1
        self.edges[e] = multiplicity
        if frontier_edge:
            self.frontier_edges.append(e)

    def delete_edge(self, e):
        multiplicity = self.edges[e]
        multiplicity -= 1
        if multiplicity == 0:
            del self.edges[e]
        else:
            self.edges[e] = multiplicity

    def degree(self):
        return sum(self.edges.values())

    def even_degree(self):
        return self.degree() % 2 == 0

    def has_frontier_edge(self, searched_e):
        """
        returns true if one of our frontier edges
        is given edge
        """
        for e in self.frontier_edges:
            if e.is_same(searched_e):
                return True
        return False

    def has_frontier_edges_on_different_sides_of(self, y):
        """
        returns true if our frontier edges
        are on different sides of horizontal line
        at given y
        """
        aboves = [e.is_above_y(y) for e in self.frontier_edges]
        return (aboves[0] != aboves[1])

    def other_frontier_edge(self, avoided_e):
        """
        returns edge in frontier edges
        which is not given edge
        """
        for e in self.frontier_edges:
            if not e.is_same(avoided_e):
                return e
        raise Exception("no different edge")

    def get_non_horizontal_frontier_edge(self):
        """
        returns edge in frontier edges
        which is not horizontal
        """
        for e in self.frontier_edges:
            if not e.is_horizontal():
                return e
        raise Exception("only horizontal edges")

    def find_first_neighbor_not(self, neighbor):
        for e in self.edges.keys():
            if neighbor is None or e.get_endpoint(1) != neighbor:
                return e
        raise Exception("only one neighbor")

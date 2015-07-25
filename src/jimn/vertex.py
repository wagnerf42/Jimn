from jimn.point import point


class vertex(point):
    def __init__(self, position_point):
        super().__init__(position_point.get_coordinates())
        self.edges = []

    def get_bounding_box(self):
        box = super(vertex, self).get_bounding_box()
        for e in self.edges:
            edge_box = e.get_bounding_box()
            box.update(edge_box)
        return box

    def save_svg_content(self, display, color):
        super(vertex, self).save_svg_content(display, color)
        for e in self.edges:
            e.save_svg_content(display, color)

    def get_edges(self):
        return self.edges

    def get_edge(self, index):
        return self.edges[index]

    def remove_any_edge(self):
        return self.edges.pop()

    def add_edge(self, edge):
        self.edges.append(edge)

    def delete_edge(self, edge):
        if edge in self.edges:
            self.edges.remove(edge)
        else:
            assert edge.reverse() in self.edges
            self.edges.remove(edge.reverse())

    def degree(self):
        return len(self.edges)

    def even_degree(self):
        return self.degree() % 2 == 0

    def has_edge(self, edge):
        """
        returns true if one of our first two edges
        is given edge
        """
        for e in self.edges[:2]:
            if e.is_same(edge):
                return True
        return False

    def has_edges_on_different_sides_of(self, y):
        """
        returns true if our first two edges
        are on different sides of horizontal line
        at given y
        """
        aboves = [e.is_above_y(y) for e in self.edges[:2]]
        print("aboves", aboves)
        return (aboves[0] != aboves[1])

    def other_edge(self, edge):
        """
        returns edge in first two edges
        which is not given edge
        """
        for e in self.edges[:2]:
            if not e.is_same(edge):
                return e
        raise Exception("no different edge")

    def get_non_horizontal_edge(self):
        """
        returns edge in first two edges
        which is not horizontal
        """
        for e in self.edges[:2]:
            if not e.is_horizontal():
                return e
        raise Exception("only horizontal edges")

    def find_first_neighbor_not(self, neighbor):
        for e in self.edges:
            if neighbor is None or e.get_endpoint(1) != neighbor:
                return e
        raise Exception("only one neighbor")

from jimn.point import point
from jimn.segment import are_traversing


class vertex(point):
    def __init__(self, position_point):
        super().__init__(position_point.get_coordinates())
        self.edges = []

    def get_edges(self):
        return self.edges

    def add_edge(self, edge):
        self.edges.append(edge)

    def edges_number(self):
        return len(self.edges)

    def has_horizontal_path(self):
        for e in self.edges:
            if e.is_horizontal():
                return True
        return False

    def get_non_horizontal_path(self):
        assert len(self.edges) == 2
        for e in self.edges:
            if not e.is_horizontal():
                return e
        raise Exception("only horizontal paths found")

    def is_traversed_by_paths(self):
        assert len(self.edges) == 2
        e1, e2 = self.edges
        return are_traversing(e1, e2.reverse())

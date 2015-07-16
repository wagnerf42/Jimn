from jimn.point import point


class vertex(point):
    def __init__(self, position_point):
        super().__init__(position_point.get_coordinates())
        self.edges = []

    def add_edge(self, edge):
        self.edges.append(edge)

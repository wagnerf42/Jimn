from jimn.point import point


class vertex(point):
    def __init__(self, position_point, neighbors=[]):
        super().__init__(position_point.get_coordinates())
        self.neighbors = neighbors

    def add_neighbor(self, neighbor):
        self.neighbors.append(neighbor)

from jimn.point import point


class vertex(point):
    def __init__(self, coordinates, neighbors=[]):
        super().__init__(coordinates)
        self.neighbors = neighbors

    def add_neighbor(self, neighbor):
        self.neighbors.append(neighbor)

# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4


class bounding_box:
    def __init__(self, min_coordinates, max_coordinates):
        self.min_coordinates = list(min_coordinates)
        self.max_coordinates = list(max_coordinates)

    @classmethod
    def empty_box(cls, dimension):
        min_coordinates = []
        max_coordinates = []
        for i in range(dimension):
            min_coordinates.append(float('+inf'))
            max_coordinates.append(float('-inf'))
        return cls(min_coordinates, max_coordinates)

    def add_point(self, p):
        assert p.dimension() == len(self.min_coordinates), "invalid point size"
        for i, c in enumerate(p.get_coordinates()):
            if c < self.min_coordinates[i]:
                self.min_coordinates[i] = c
            if c > self.max_coordinates[i]:
                self.max_coordinates[i] = c

    def contains_point(self, p):
        assert p.dimension() == len(self.min_coordinates), "invalid point size"
        for i, c in enumerate(p.get_coordinates()):
            if c < self.min_coordinates[i]:
                return False
            if c > self.max_coordinates[i]:
                return False
        return True

    def update(self, other):
        assert len(self.min_coordinates) == len(other.min_coordinates), 'merge different boxes'
        for i, c in enumerate(other.min_coordinates):
            if self.min_coordinates[i] > c:
                self.min_coordinates[i] = c
        for i, c in enumerate(other.max_coordinates):
            if self.max_coordinates[i] < c:
                self.max_coordinates[i] = c

    def limits(self, index):
        return (self.min_coordinates[index], self.max_coordinates[index])

    def get_arrays(self):
        return (self.max_coordinates, self.min_coordinates)

    def __str__(self):
        return('[{}]-[{}]'.format(self.min_coordinates, self.max_coordinates))

# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4


class bounding_box:
    def __init__(self, min_coordinates, max_coordinates):
        self.min_coordinates = min_coordinates
        self.max_coordinates = max_coordinates

    @classmethod
    def empty_box(cls, dimension):
        min_coordinates = []
        max_coordinates = []
        for i in range(dimension):
            min_coordinates.append(float('+inf'))
            max_coordinates.append(float('-inf'))
        return cls(min_coordinates, max_coordinates)

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

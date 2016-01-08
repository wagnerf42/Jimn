
"""
bounding boxes are rectangular boxes
delimiting a set of items.
they are usually used in display to compute image sizes.
"""


class bounding_box:
    def __init__(self, min_coordinates, max_coordinates):
        self.min_coordinates = list(min_coordinates)
        self.max_coordinates = list(max_coordinates)

    @classmethod
    def empty_box(cls, dimension):
        """
        return an empty box in space of given dimension
        """
        min_coordinates = []
        max_coordinates = []
        for _ in range(dimension):
            min_coordinates.append(float('+inf'))
            max_coordinates.append(float('-inf'))
        return cls(min_coordinates, max_coordinates)

    def add_point(self, added_point):
        """
        register a point inside the box.
        update box limits if needed.
        """
        for i, added_coordinate in enumerate(added_point.get_coordinates()):
            if added_coordinate < self.min_coordinates[i]:
                self.min_coordinates[i] = added_coordinate
            if added_coordinate > self.max_coordinates[i]:
                self.max_coordinates[i] = added_coordinate

    def almost_contains_point(self, p):
        """
        returns true if point p is almost inside box
        """
        assert p.dimension() == len(self.min_coordinates), "invalid point size"
        for i, c in enumerate(p.get_coordinates()):
            if c < self.min_coordinates[i] and \
                    (not is_almost(c, self.min_coordinates[i])):
                return False
            if c > self.max_coordinates[i] and \
                    (not is_almost(c, self.max_coordinates[i])):
                return False
        return True

    def intersects(self, other):
        """
        returns if the two boxes intersect (even only on edge)
        """
        for d in range(len(self.min_coordinates)):
            small = self.min_coordinates[d]
            big = self.max_coordinates[d]
            other_small = other.min_coordinates[d]
            other_big = other.max_coordinates[d]
            if (other_small > big) or (other_big < small):
                return False
        return True

    def update(self, other):
        """
        update self box by taking constraints from other box into account
        """
        assert len(self.min_coordinates) == len(other.min_coordinates), \
            'merge different boxes'
        for i, c in enumerate(other.min_coordinates):
            if self.min_coordinates[i] > c:
                self.min_coordinates[i] = c
        for i, c in enumerate(other.max_coordinates):
            if self.max_coordinates[i] < c:
                self.max_coordinates[i] = c

    def limits(self, index):
        """
        returns array of limits for a given coordinate index
        """
        return (self.min_coordinates[index], self.max_coordinates[index])

    def inflate(self, distance):
        """
        get bigger box containing original box + any point outside
        original at distance less than given
        """
        self.min_coordinates = [c - distance for c in self.min_coordinates]
        self.max_coordinates = [c + distance for c in self.max_coordinates]

    def get_arrays(self):
        """
        returns arrays of limits
        """
        return (self.min_coordinates, self.max_coordinates)

    def __str__(self):
        return('[{}]-[{}]'.format(self.min_coordinates, self.max_coordinates))

from jimn.utils.precision import is_almost

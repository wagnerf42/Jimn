"""
hash nearby points together in O(1).
"""
from copy import copy
from jimn.utils.precision import coordinate_key, displaced_coordinate_key, \
    precision


class CoordinatesHash:
    """
    hash a set of points by their rounded coordinates.
    """
    def __init__(self, wanted_precision=precision):
        self.hashes = [{}, {}]
        self.precision = wanted_precision

    def hash_point(self, point):
        """
        add a point to the hash.
        if a nearby point is already hashed do not hash but return nearby point.
        """
        key = ";".join([coordinate_key(c, self.precision)
                        for c in point.coordinates])
        displaced_key = ";".join([displaced_coordinate_key(c, self.precision)
                                  for c in point.coordinates])

        if key in self.hashes[0]:
            return self.hashes[0][key]
        if displaced_key in self.hashes[1]:
            return self.hashes[1][displaced_key]

        self.hashes[0][key] = point
        self.hashes[1][displaced_key] = point
        return copy(point)

    def hash_coordinate(self, coordinate):
        """
        same as hash_point but with 1d points.
        """
        key = coordinate_key(coordinate, self.precision)
        displaced_key = displaced_coordinate_key(coordinate, self.precision)
        if key in self.hashes[0]:
            return self.hashes[0][key]
        if displaced_key in self.hashes[1]:
            return self.hashes[1][displaced_key]

        self.hashes[0][key] = coordinate
        self.hashes[1][displaced_key] = coordinate
        return coordinate

ROUNDER2D = CoordinatesHash()
LINES_ROUNDER = CoordinatesHash(precision-2)

"""
hash nearby points together in O(1).
"""
from copy import copy
from jimn.utils.precision import coordinate_key, displaced_coordinate_key, \
    PRECISION


class CoordinatesHash:
    """
    hash a set of points by their rounded coordinates.
    """
    def __init__(self, wanted_precision=PRECISION):
        self.hashes = [{}, {}]
        self.precision = wanted_precision
        self.fast_hash = set()  # fast test for exact match

    def hash_point(self, point):
        """
        add a point to the hash.
        if a nearby point is already hashed do not hash but return nearby point.
        """
        if point in self.fast_hash:
            return point
        key = ";".join([coordinate_key(c, self.precision)
                        for c in point.coordinates])
        displaced_key = ";".join([displaced_coordinate_key(c, self.precision)
                                  for c in point.coordinates])

        if key in self.hashes[0]:
            return copy(self.hashes[0][key])
        if displaced_key in self.hashes[1]:
            return copy(self.hashes[1][displaced_key])

        self.hashes[0][key] = point
        self.hashes[1][displaced_key] = point
        self.fast_hash.add(point)
        return point

    def hash_coordinate(self, coordinate):
        """
        same as hash_point but with 1d points.
        """
        if coordinate in self.fast_hash:
            return coordinate
        key = coordinate_key(coordinate, self.precision)
        displaced_key = displaced_coordinate_key(coordinate, self.precision)
        if key in self.hashes[0]:
            return self.hashes[0][key]
        if displaced_key in self.hashes[1]:
            return self.hashes[1][displaced_key]

        self.hashes[0][key] = coordinate
        self.hashes[1][displaced_key] = coordinate
        self.fast_hash.add(coordinate)
        return coordinate

    def contains_coordinate(self, coordinate):
        """
        do we contain given coordinate ?
        prerequisite: self is 1d-hash.
        """
        if coordinate in self.fast_hash:
            return True
        key = coordinate_key(coordinate, self.precision)
        if key in self.hashes[0]:
            return True
        displaced_key = displaced_coordinate_key(coordinate, self.precision)
        if displaced_key in self.hashes[1]:
            return True
        return False

ROUNDER2D = CoordinatesHash()
LINES_ROUNDER = CoordinatesHash(PRECISION-2)

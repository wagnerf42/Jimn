"""
hash nearby points together in O(1).
"""
from jimn.point import Point
from jimn.utils.precision import coordinate_key, displaced_coordinate_key, \
    PRECISION


class CoordinatesHash:
    """
    hash a set of points by their rounded coordinates.
    """
    def __init__(self, dimension=2, wanted_precision=PRECISION):
        self.hashes = [({}, {}) for _ in range(dimension)]
        self.precision = wanted_precision
        self.fast_hash = set()  # fast test for exact match

    def hash_point(self, point):
        """
        add a point to the hash.
        if a nearby point is already hashed
        do not hash but return nearby point.
        """
        if point in self.fast_hash:
            return point
        # TODO: trop moche
        keys = [
            [coordinate_key(c, self.precision) for c in point.coordinates],
            [displaced_coordinate_key(c, self.precision)
             for c in point.coordinates]
        ]

        new_coordinates = list(point.coordinates)
        for index in range(len(new_coordinates)):
            # update
            for hash_index, coordinates_hash in enumerate(self.hashes[index]):
                key = keys[hash_index][index]
                if key in coordinates_hash:
                    new_coordinates[index] = coordinates_hash[key]
                    break

            # re-hash
            for hash_index, coordinates_hash in enumerate(self.hashes[index]):
                key = keys[hash_index][index]
                coordinates_hash[key] = new_coordinates[index]

        hashed_point = Point(new_coordinates)
        self.fast_hash.add(hashed_point)
        return hashed_point

    def hash_coordinate(self, coordinate):
        """
        same as hash_point but with 1d points.
        """
        raise Exception("REDO")
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
LINES_ROUNDER = CoordinatesHash(wanted_precision=PRECISION-2)

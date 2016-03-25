"""
hash nearby points together in O(1).
"""
from copy import copy
from jimn.utils.precision import coordinate_key, displaced_coordinate_key, \
    PRECISION


class CoordinatesHash:
    """
    a CoordinatesHash is structure providing a very fast way (O(1)) of
    merging nearby coordinates in points.
    when initializing a new hash you need to provide the dimension of the space
    and a wanted_precision.

    we ensure that :
        - if any two points have for all coordinates a difference less than
        0.5*10**-wanted_precision then they will be merged to the value of the
        most ancient in the hash.
        - if two points are merged then IN GENERAL their coordinates'
        differences are less than 2*10**-wanted_precision (transitivity might
        increase this value but it is very unlikely).
    """
    def __init__(self, wanted_precision=PRECISION, dimension=2):
        # we need 2**dimension hashes to test all combinations
        # of displaced and non displaced keys
        # in hash number n : each bit means key for corresponding
        # dimension is displaced
        # (hash 0 : nothing displaced ; hash 1 : first dimension's coordinate
        # is displaced, ....)
        self.hashes = [{} for _ in range(2**dimension)]
        self.precision = wanted_precision
        self.fast_hash = set()  # fast test for exact match

    def hash_point(self, point):
        """
        add a point to the hash.
        if a nearby point is already hashed do not hash but return
        nearby point.
        """
        if point in self.fast_hash:
            return point

        # lookup
        keys = []
        for key_index, points_hash in enumerate(self.hashes):
            key = self._compute_key(key_index, point)
            keys.append(key)
            if key in points_hash:
                return copy(points_hash[key])

        # new point ; store it
        for key, points_hash in zip(keys, self.hashes):
            points_hash[key] = point

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

    def _compute_key(self, hash_number, point):
        """
        compute key for given point for use in hash numbered hash number.
        """
        key_parts = []
        remaining_bits = hash_number
        for coordinate in point.coordinates:
            if remaining_bits % 2:
                key_parts.append(
                    displaced_coordinate_key(coordinate, self.precision))
            else:
                key_parts.append(coordinate_key(coordinate, self.precision))

            remaining_bits //= 2

        key = ";".join(key_parts)
        return key


ROUNDER2D = CoordinatesHash(PRECISION-1)  # we take precision-1 to ensure
# all almost same points hash together
LINES_ROUNDER = CoordinatesHash(PRECISION-2)

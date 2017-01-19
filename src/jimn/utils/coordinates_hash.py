"""
hash nearby points together in O(1).
"""
from jimn.utils.precision import coordinate_key, displaced_coordinate_key, \
    PRECISION
from jimn.point import Point


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
        self.hashes = [{} for _ in range(2*dimension)]
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

        new_coordinates = [self.hash_coordinate(c, i) for i, c in enumerate(point.coordinates)]
        new_point = Point(new_coordinates)

        self.fast_hash.add(new_point)
        return new_point

    def hash_coordinate(self, coordinate, index=0):
        """
        same as hash_point but with 1d points.
        """
        key = coordinate_key(coordinate, self.precision)
        displaced_key = displaced_coordinate_key(coordinate, self.precision)
        if key in self.hashes[2*index]:
            return self.hashes[2*index][key]
        if displaced_key in self.hashes[2*index+1]:
            return self.hashes[2*index+1][displaced_key]

        self.hashes[2*index][key] = coordinate
        self.hashes[2*index+1][displaced_key] = coordinate
        return coordinate

    def contains_coordinate(self, coordinate, index=0):
        """
        do we contain given coordinate ?
        prerequisite: self is 1d-hash.
        """
        key = coordinate_key(coordinate, self.precision)
        if key in self.hashes[index]:
            return True
        displaced_key = displaced_coordinate_key(coordinate, self.precision)
        if displaced_key in self.hashes[index+1]:
            return True
        return False

    def clear(self):
        """
        remove all content.
        """
        for coordinate_hash in self.hashes:
            coordinate_hash.clear()
        self.fast_hash.clear()


ROUNDER2D = CoordinatesHash(PRECISION-1)  # we take precision-1 to ensure
# all almost same points hash together
LINES_ROUNDER = CoordinatesHash(PRECISION-2)

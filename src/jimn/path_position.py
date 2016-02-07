"""
module for recording a position on a path.
we can use it to compute positions on small paths ( = fast)
then change the path by adding new elementary paths ( after the position )
and finally use the stored position on the long path without recomputing it.
"""
from jimn.caching import cached


class PathPosition:
    # pylint: disable=too-few-public-methods
    """
    position on a path (contains index of elementary path on path).
    """
    def __init__(self, elementary_path, position_point, index):
        """
        creates an object storing a position on some path.
        always fast modifications of path at given position later on.
        TRICKY TO USE : be careful that index in position is not
        update on path change
        """
        self.point = position_point
        self.elementary_path = elementary_path
        self.index = index

    @cached
    def distance(self):
        """
        return distance of position from start of elementary path
        containing it.
        """
        return self.elementary_path.distance_from_start(self.point)

    def __lt__(self, other):
        """
        compares to position on same path.
        true if self is reached before other when following path from its start
        """
        if self.index < other.index:
            return True
        if self.index > other.index:
            return False
        return self.distance() < other.distance()

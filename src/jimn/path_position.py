from jimn.caching import cached

class path_position:
    def __init__(self, position_point, ep, index):
        """
        creates an object storing a position on some path.
        always fast modifications of path at given position later on.
        TRICKY TO USE : be careful that index in position is not
        update on path change
        """
        self.point = position_point
        self.ep = ep
        self.index = index

    @cached
    def distance(self):
        return self.ep.squared_distance_from_start(self.point)

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

from jimn.displayable import tycat

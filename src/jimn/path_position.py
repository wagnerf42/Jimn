class path_position:
    def __init__(self, outer_point, inner_point, ep, outer_index, inner_index):
        """
        creates an object storing a interference position on two paths.
        records:
            - point on the followed path
            - interference point on the other path
            - followed elementary path (on outer path)
            - the index of the elementary path in the followed path
        """
        self.outer_point = outer_point
        self.inner_point = inner_point
        self.ep = ep
        if outer_index >= 0:
            self.distance = ep.squared_distance_from_start(outer_point)
        self.outer_index = outer_index
        self.inner_index = inner_index
        if __debug__:
            if self.ep:
                assert ep.contains(outer_point)

    @classmethod
    def empty_position(cls):
        """
        returns a non existing position which will always compare as
        less than a real one
        """
        return cls(None, None, None, -1)

    def is_not_empty(self):
        """
        returns true if position really contains something
        """
        return self.index >= 0

    def __lt__(self, other):
        """
        compares to position on same path.
        true if self is reached before other when following path from its start
        """
        if self.index < other.index:
            return True
        if self.index > other.index:
            return False
        return self.distance < other.distance



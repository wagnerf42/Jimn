"""
position simultaneous on two different paths.
"""
from jimn.path_position import PathPosition


class DualPosition:
    # pylint: disable=too-few-public-methods
    """
    position simultaneous on two different paths.
    we can use it to mark on position impacted by mill on path2
    while following path1.
    """
    def __init__(self, outer_path, outer_point, inner_point, outer_index):
        """
        stores positions on two different paths.
        used when switching between different paths.
        """
        self.outer_position = PathPosition(outer_path,
                                           outer_point, outer_index)
        self.inner_position = PathPosition(None, inner_point, None)

    def __lt__(self, other):
        """
        true if self's outer position is closer from start of path
        than other's outer position.
        both outer positions need to be on same outer path.
        """
        return self.outer_position < other.outer_position

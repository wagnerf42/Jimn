"""
kuhn munkres intersection algorithm.
"""

from collections import defaultdict
from jimn.point import Point
from jimn.segment import Segment
from jimn.tree.treap import Treap
from jimn.algorithms.sweeping_line_algorithms import SweepingLineAlgorithm


class KuhnMunkres(SweepingLineAlgorithm):
    """
    this class computes all intersections in a set of paths.
    return a defaultdict with given paths as keys at for each
    of them a list of intersections found as values.
    """
    def __init__(self, paths):
        self.intersections = defaultdict(list)
        self.crossed_segments = Treap(Segment([Point([-2000, 2000]), Point([2000, 2000])]),
                                      root_node=True)
        super().__init__(paths)

    def add_path(self, path):
        pass

    def remove_path(self, path):
        pass


def kuhn_munkres(paths):
    """
    intersect a set of paths.
    return a defaultdict with paths as keys and intersections as values.
    """
    intersecter = KuhnMunkres(paths)
    return intersecter.intersections

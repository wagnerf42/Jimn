"""
kuhn munkres intersection algorithm.
"""

from collections import defaultdict
from jimn.algorithms.sweeping_line_algorithms import SweepingLineAlgorithm


class KuhnMunkres(SweepingLineAlgorithm):
    """
    this class computes all intersections in a set of paths.
    return a defaultdict with given paths as keys at for each
    of them a list of intersections found as values.
    """
    def __init__(self, paths):
        self.intersections = defaultdict(list)
        super().__init__(paths)

    def add_path(self, path):
        node = self.crossed_paths.add(path)
        for neighbour in node.neighbours():
            neighbour_path = neighbour.content
            self._try_intersecting(path, neighbour_path)

    def remove_path(self, path):
        node = self.crossed_paths.find(path)
        neighbour_paths = [n.content for n in node.neighbours()]
        self._try_intersecting(*neighbour_paths)
        node.remove()

    def _try_intersecting(self, *paths):
        intersections = paths[0].intersections_with(paths[1])
        for intersection in intersections():
            intersected_paths = [
                p for p in paths
                if not (p.endpoints[0].is_almost(intersection)
                        or p.endpoints[1].is_almost(intersection))
            ]
            if intersected_paths:
                self.add_crossing_event(intersection, intersected_paths)


def kuhn_munkres(paths):
    """
    intersect a set of paths.
    return a defaultdict with paths as keys and intersections as values.
    """
    intersecter = KuhnMunkres(paths)
    return intersecter.intersections

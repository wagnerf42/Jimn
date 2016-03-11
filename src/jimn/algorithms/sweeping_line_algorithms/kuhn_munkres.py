"""
kuhn munkres intersection algorithm.
"""

from jimn.displayable import tycat
from jimn.algorithms.sweeping_line_algorithms import SweepingLineAlgorithm
from jimn.utils.coordinates_hash import ROUNDER2D


class KuhnMunkres(SweepingLineAlgorithm):
    """
    this class computes all intersections in a set of paths.
    """
    def __init__(self, paths):
        self.cut_paths = []  # results
        self.terminated_paths = set()  # paths cancelled before their endpoint
        super().__init__(paths)

    def add_paths(self, paths):
        """
        new paths found. add them to set of paths.
        mark possible intersections around them.
        compute intersections.
        """
        for path in paths:
            node = self.crossed_paths.add(path)
            for neighbour in node.neighbours():
                self._try_intersecting((node, neighbour))

    def remove_paths(self, paths):
        """
        paths end. remove them from set of paths.
        mark possible intersections around them.
        """
        assert len(set(paths)) == len(paths), "double removal"
        for path in paths:
            if path not in self.terminated_paths:
                self.cut_paths.append(path)
                try:
                    node = self.crossed_paths.find_object(path)
                except:
                    print("failed finding", path,
                          "in tree, at", self.current_x)
                    tycat(*self.crossed_paths.ordered_contents())
                    tycat(self.crossed_paths.ordered_contents(), path)
                    self.crossed_paths.tycat()
                    self.crossed_paths.debug_find(path)
                    raise

                neighbours = node.neighbours()
                if len(neighbours) == 2:
                    self._try_intersecting(neighbours)

                node.remove()

    def _try_intersecting(self, nodes):
        """
        check possible intersections.
        """
        paths = [n.content for n in nodes]
        intersections = paths[0].intersections_with(paths[1])
        if not intersections:
            return
        intersections = [ROUNDER2D.hash_point(i) for i in intersections]
        if __debug__:
            for intersection in intersections:
                if intersection.get_x() < self.current_x:
                    raise Exception("backward intersection")

        for index, path in enumerate(paths):
            small_paths = path.split_at(intersections)
            if len(small_paths) > 1:
                # replace big path by small paths
                nodes[index].remove()
                self.terminated_paths.add(path)
                # handle all small paths
                for path in small_paths:
                    x_coordinates = list(sorted([p.get_x()
                                                 for p in path.endpoints]))
                    if x_coordinates[1] == self.current_x:
                        # if ending here : add it to results
                        self.cut_paths.append(path)
                    elif x_coordinates[0] > self.current_x:
                        # if starting later : add as event
                        self.add_path_events(path, x_coordinates)
                    else:
                        # it started before : add it back
                        self.crossed_paths.add(path)
                        self.add_end_event(path, x_coordinates[1])


def kuhn_munkres(paths):
    """
    intersect a set of paths.
    return a defaultdict with paths as keys and intersections as values.
    """
    intersecter = KuhnMunkres(paths)
    return intersecter.cut_paths

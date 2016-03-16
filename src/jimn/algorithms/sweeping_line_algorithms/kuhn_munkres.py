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
        """
        for path in paths:
            self.add_path(path)

    def add_path(self, path):
        """
        new path found. add it to set of paths.
        """
        node = self.crossed_paths.add(path)
        small_neighbour = node.nearest_node(False)
        if small_neighbour is None or\
                not self._try_intersecting((small_neighbour, node)):
            big_neighbour = node.nearest_node(True)
            if big_neighbour is not None:
                self._try_intersecting((node, big_neighbour))

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
                    self.tycat()
                    tycat(self.crossed_paths.ordered_contents(), path)
                    self.crossed_paths.debug_find(path)
                    raise

                neighbours = node.neighbours()
                node.remove()

                if len(neighbours) == 2:
                    self._try_intersecting(neighbours)

    def tycat(self):
        """
        display current state.
        """
        tycat(self.cut_paths, *self.crossed_paths.ordered_contents())
        self.crossed_paths.tycat()

    def _try_intersecting(self, nodes):
        """
        check possible intersections.
        return if any intersection.
        pre-condition : node[0].content < node[1].content
        """
        paths = [n.content for n in nodes]
        intersection = paths[0].intersection_with_segment(paths[1])
        if intersection is None:
            return False
        intersection = ROUNDER2D.hash_point(intersection)

        smaller_than_small_node = nodes[0].nearest_node(False)
        bigger_than_big_node = nodes[1].nearest_node(True)
        for node in nodes:
            node.remove()
            self.terminated_paths.add(node.content)

        # when crossing you need to test for intersections on the other side
        self._split_path(paths[0], intersection, bigger_than_big_node, True)
        self._split_path(paths[1], intersection,
                         smaller_than_small_node, False)

        return True

    def _split_path(self, path, split_point, neighbour_node, big_neighbour):
        """
        split path into chunks. add them back into system, eventually
        intersecting neighbour.
        we need as information:
            - the path to split
            - the splitting point
            - the neighbour which might be intersected after crossing
            - the direction of this neighbour from path
        """
        if path.endpoints[0] < path.endpoints[1]:
            start, end = path.split_around(split_point)
        else:
            end, start = path.split_around(split_point)

        if split_point.get_x() == self.current_x:
            if start is not None:
                self.cut_paths.append(start)
            if end is not None:
                # it starts here : add it to tree
                node = self.crossed_paths.add(end)
                # check intersection with neighbour right now
                if neighbour_node is not None:
                    if big_neighbour:
                        self._try_intersecting((node, neighbour_node))
                    else:
                        self._try_intersecting((neighbour_node, node))
        else:
            if start is not None:
                self.crossed_paths.add(start)
                self.add_end_event(start, split_point.get_x())
            if end is not None:
                # if starting later : add as event
                coordinates = sorted([p.get_x() for p in end.endpoints])
                self.add_path_events(end, coordinates)


def kuhn_munkres(paths):
    """
    intersect a set of paths.
    return a defaultdict with paths as keys and intersections as values.
    """
    intersecter = KuhnMunkres(paths)
    return intersecter.cut_paths

"""
kuhn munkres intersection algorithm.
"""

from jimn.displayable import tycat
from jimn.algorithms.sweeping_line_algorithms import SweepingLineAlgorithm
from jimn.utils.coordinates_hash import ROUNDER2D
from jimn.utils.debug import is_module_debugged
from jimn.tree.treap import ConflictingKeys


class KuhnMunkres(SweepingLineAlgorithm):
    """
    this class computes all intersections in a set of paths.
    """
    def __init__(self, paths):
        self.cut_paths = []  # results
        self.terminated_paths = set()  # paths cancelled before their endpoint
        # we need it since we cannot remove cancel events from the heap
        super().__init__(paths)

    def add_paths(self, paths):
        """
        new paths found. add them to set of paths.
        """
        for path in paths:
            try:
                self._add_path(path)
            except ConflictingKeys as conflict:
                # we have overlapping paths. we need to remove overlap.
                # 1) figure out what is left
                chunks = conflict.existing_node.content.remove_overlap_with(
                    conflict.new_content
                )
                # remove everyone from tree and events
                self.terminated_paths.add(conflict.existing_node.content)
                self.terminated_paths.add(conflict.new_content)
                conflict.existing_node.remove()
                # add chunks back to the system
                self._handle_chunks(chunks)

    def remove_paths(self, paths):
        """
        paths end. remove them from set of paths.
        mark possible intersections around them.
        """
        assert len(set(paths)) == len(paths)

        for path in paths:
            if path not in self.terminated_paths:
                self.cut_paths.append(path)
                self._remove_path(path)

    def tycat(self):
        """
        display current state.
        zoom on current point and display graph.
        """
        tycat(self.cut_paths, self.current_point,
              *self.crossed_paths.ordered_contents())
        tycat([s.clip(self.current_point, 0.01)
               for s in self.cut_paths],
              self.current_point,
              *[s.clip(self.current_point, 0.01)
                for s in self.crossed_paths.ordered_contents()])
        self.crossed_paths.tycat()

    def _add_path(self, path):
        """
        new path found. add it to set of paths.
        """
        node = self.crossed_paths.add(path)
        neighbour, intersection = self._nearest_intersecting_neighbour(
            node)
        if intersection is not None:
            self._intersect_nodes((node, neighbour), intersection)

    def _nearest_intersecting_neighbour(self, node):
        """
        return amongst all neighbours the one with the nearest intersection
        (together with the intersection).
        """
        neighbours = node.neighbours()
        if not neighbours:
            return (None, None)

        if len(neighbours) == 1:
            return (neighbours[0], self._find_intersection((node,
                                                            neighbours[0])))

        intersections = [self._find_intersection((node, n))
                         for n in neighbours]
        if intersections[0] is None and intersections[1] is None:
            return (None, None)
        if intersections[0] is None:
            selected = 1
        elif intersections[1] is None:
            selected = 0
        else:
            if intersections[0] < intersections[1]:
                selected = 0
            else:
                selected = 1
        return (neighbours[selected], intersections[selected])

    def _remove_path(self, path):
        """
        remove given path from tree and intersect neighbours.
        """
        try:
            node = self.crossed_paths.find_object(path)
        except:
            print("failed finding", path, "in tree, at", self.current_point)
            self.tycat()
            tycat(self.crossed_paths.ordered_contents(), path)
            self.crossed_paths.debug_find(path)
            raise

        neighbours = node.neighbours()
        node.remove()

        if len(neighbours) == 2:
            intersection = self._find_intersection(neighbours)
            if intersection is not None:
                self._intersect_nodes(neighbours, intersection)

    def _find_intersection(self, nodes):
        """
        check possible intersections.
        """
        paths = [n.content for n in nodes]
        intersection = paths[0].intersection_with_segment(paths[1])
        if intersection is None:
            return
        intersection = ROUNDER2D.hash_point(intersection)
        if intersection in paths[0].endpoints and \
                intersection in paths[1].endpoints:
            # not really an intersection
            return
        if __debug__:
            if is_module_debugged(__name__):
                print("intersecting:",
                      [str(p) for p in paths], "at", intersection)
                tycat([s.clip(intersection, 0.00001)
                       for s in self.cut_paths],
                      intersection,
                      *[s.clip(intersection, 0.00001)
                        for s in self.crossed_paths.ordered_contents()])
        return intersection

    def _intersect_nodes(self, nodes, intersection):
        """
        apply given intersection to both nodes.
        pre-condition: intersection is in both nodes' paths.
        """
        for node in nodes:
            self._split_path(node, intersection)

        if __debug__:
            if is_module_debugged(__name__):
                print("after intersection:")
                tycat([s.clip(intersection, 0.01)
                       for s in self.cut_paths],
                      intersection,
                      *[s.clip(intersection, 0.01)
                        for s in self.crossed_paths.ordered_contents()])

    def _is_chunk_completed(self, chunk):
        """
        return if given path chunk is ending before current point.
        """
        return max(chunk.endpoints) <= self.current_point

    def _is_chunk_started(self, chunk):
        """
        return if given path chunk is started before current point.
        """
        return min(chunk.endpoints) <= self.current_point

    def _handle_chunks(self, chunks):
        """
        some paths changed. handle their remains
        (decide whether or not to insert them now, later, never)
        """
        for chunk in chunks:
            if self._is_chunk_completed(chunk):
                self.cut_paths.append(chunk)
            elif self._is_chunk_started(chunk):
                self.crossed_paths.add(chunk)
                self.add_end_event(chunk, max(chunk.endpoints))
            else:
                self.add_path_events(chunk, sorted(chunk.endpoints))

    def _split_path(self, node, split_point):
        """
        split node's path into chunks.
        """
        path = node.content
        if path.endpoints[0] < path.endpoints[1]:
            start, end = path.split_around(split_point)
        else:
            end, start = path.split_around(split_point)

        if start is None or end is None:
            return  # this path is not really intersected

        node.remove()
        self.terminated_paths.add(path)

        if __debug__:
            if split_point < self.current_point:
                print("current point is", self.current_point)
                print("split point is", split_point)
                tycat([s.clip(split_point, 0.01)
                       for s in self.cut_paths],
                      split_point,
                      self.current_point,
                      *[s.clip(split_point, 0.01)
                        for s in self.crossed_paths.ordered_contents()])
                self.crossed_paths.tycat()
                # raise Exception("intersection going back")

        if split_point == self.current_point:
            # start is finished and end is already started
            self.cut_paths.append(start)
            node = self.crossed_paths.add(end)
            self.add_end_event(end, max(end.endpoints))
        else:
            # start is not yet finished and end is not started
            self.crossed_paths.add(start)
            self.add_end_event(start, split_point)
            self.add_path_events(end, sorted(end.endpoints))


def kuhn_munkres(paths):
    """
    intersect a set of paths.
    return a defaultdict with paths as keys and intersections as values.
    """
    intersecter = KuhnMunkres(paths)
    return intersecter.cut_paths

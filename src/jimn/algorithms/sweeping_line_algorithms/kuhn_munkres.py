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
    def __init__(self, paths, cut_arcs=False):
        self.cut_paths = []  # results
        self.terminated_paths = set()  # paths cancelled before their endpoint
        # we need it since we cannot cancel events from the heap
        super().__init__(paths, cut_arcs)

    def add_paths(self, paths):
        """
        new paths found. add them to set of paths.
        """
        for path in paths:
            if path in self.terminated_paths:
                continue  # happens on intersections
            try:
                self._add_path(path)
            except ConflictingKeys as conflict:
                # we have overlapping paths. we need to remove overlap.
                # 1) figure out what is left
                tycat(conflict.existing_node.content, conflict.new_content)
                raise Exception("no way")
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
        for path in paths:
            if path not in self.terminated_paths:
                # overlapped paths can appear twice or more in paths
                # but will be terminated before reaching inside
                # this conditional
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
        self.terminated_paths.add(path)

        if len(neighbours) == 2:
            intersection = self._find_intersection(neighbours)
            if intersection is not None:
                self._intersect_nodes(neighbours, intersection)

    def _find_intersection(self, nodes):
        """
        check possible intersections.
        """
        paths = [n.content for n in nodes]
        intersections = paths[0].intersections_with(paths[1])
        intersections = [
            i for i in intersections
            if not i.is_almost(self.current_point) and
            i not in paths[0].endpoints and
            i not in paths[1].endpoints
        ]
        if not intersections:
            return

        # take leftmost intersection
        intersection = ROUNDER2D.hash_point(min(intersections))
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
        return max(chunk.endpoints) <= self.incoming_point

    def _is_chunk_started(self, chunk):
        """
        return if given path chunk is started before (strictly)
        current point.
        """
        return min(chunk.endpoints) < self.incoming_point

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
        chunks = path.split_around(split_point)

        if chunks[0] is None or chunks[1] is None:
            return  # this path is not really intersected
        node.remove()
        self.terminated_paths.add(path)
        self._handle_chunks(chunks)


def kuhn_munkres(paths, cut_arcs=False):
    """
    intersect a set of paths.
    return list of elementary paths.
    """
    intersecter = KuhnMunkres(paths, cut_arcs)
    return intersecter.cut_paths

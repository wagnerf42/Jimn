"""
figure out hierarchy between polygons.
"""
from collections import defaultdict
from jimn.algorithms.sweeping_line_algorithms import SweepingLineAlgorithm
from jimn.tree.inclusion_tree import InclusionTree
from jimn.utils.debug import is_module_debugged
from jimn.tree.inclusion_tree.polygonsegment import PolygonSegment


class InclusionTreeBuilder(SweepingLineAlgorithm):
    """
    this class builds a tree of polygons included in one another.
    it works through a sweeping line algorithm.
    also identifies each as a hole or a polygon.
    """
    def __init__(self, polygons):
        self.polygons = polygons
        self.current_paths = defaultdict(list)
        self.seen_polygons = set()
        self.tree = InclusionTree()
        self.fathers = {}
        super().__init__(self._create_segments())

    def _create_segments(self):
        # get all non-vertical segments in polygons
        segments = []
        for height, polygons in self.polygons.items():
            for polygon in polygons:
                segments.extend(_non_vertical_segments(polygon, height))
        return segments

    def __terminate_polygon(self, polygon_id):
        # mark polygon as dead in tree
        father = self.fathers[polygon_id]
        del father.alive_children[polygon_id]

    def add_polygon_in_tree(self, new_segment):
        """
        we meet a new segment from a never seen polygon.
        place polygon at right place in tree.
        """
        root = self.tree
        # new polygon
        # we first try inserting it at current level ; it might be a hole
        # if not we try going below
        # see report for more help on inclusion trees
        for child in sorted(list(root.alive_children.values()),
                            key=lambda c: c.height, reverse=True):
            if self.add_polygon_rec(child, new_segment):
                break
        else:
            self.add_child_in_tree(root, new_segment)

    def add_polygon_rec(self, node, new_segment):
        """
        add new polygon in tree. right position is found recursively.
        """
        # see comments in add_polygon_in_tree
        if self.is_included(new_segment, node.content):
            for child in sorted(list(node.alive_children.values()),
                                key=lambda c: c.height, reverse=True):
                if self.add_polygon_rec(child, new_segment):
                    return True

            if node.is_polygon or \
                    new_segment.height == node.height:
                self.add_child_in_tree(node, new_segment)
                return True

        return False

    def add_child_in_tree(self, node, new_segment):
        """
        add self (poly node corresponding to new_segment)
        as child of given node.
        """
        node.add_child(new_segment)
        self.fathers[new_segment.get_polygon_id()] = node

    def is_included(self, new_segment, polygon):
        """
        return if new_segment is included in given polygon.
        """
        if id(polygon) not in self.current_paths:
            # this polygon is dead
            # we cannot be included here because new_segment is alive
            return False
        else:
            segments = self.current_paths[id(polygon)]
            if segments[0].height < new_segment.height:
                return False
            # do if we are inside, we simply count number of segments
            # traversed when going in one direction (here up)
            # s1 >= s2 means s1 above and higher than s2 (not strictly)
            current_x = new_segment.endpoints[0].get_x()
            y_coordinates = [s.vertical_intersection_at(current_x)
                             for s in segments]
            limit_y = new_segment.endpoints[0].get_y()
            return len([y for y in y_coordinates if y <= limit_y]) % 2 == 1

    def add_paths(self, paths):
        """
        new paths handler. check each time if new polygon.
        """
        paths = sorted(paths, key=lambda p: p.height, reverse=True)
        for path in paths:
            polygon_id = path.get_polygon_id()
            self.current_paths[polygon_id].append(path)

            if polygon_id not in self.seen_polygons:
                # this guy is new, categorize it
                # add it in tree
                self.add_polygon_in_tree(path)

                # mark it as seen
                self.seen_polygons.add(polygon_id)
                if __debug__:
                    if is_module_debugged(__name__):
                        print("added polygon", id(path.polygon),
                              "( h =", path.height, ")")
                        self.tree.tycat()

    def remove_paths(self, paths):
        """
        remove paths handler. check each time if last of polygon.
        """
        for path in paths:
            polygon_id = path.get_polygon_id()
            self.current_paths[polygon_id].remove(path)
            if not self.current_paths[polygon_id]:
                del self.current_paths[polygon_id]
                self.__terminate_polygon(polygon_id)


def _non_vertical_segments(polygon, height):
    """
    return segments in polygon which are non vertical.
    create PolygonSegment objects at given height.
    """
    return [
        PolygonSegment(s, height, polygon)
        for s in polygon.segments()
        if not s.is_almost_vertical()
    ]


def build_inclusion_tree(polygons):
    """
    turn a set of polygons hashed by height into a polygon tree.
    """
    builder = InclusionTreeBuilder(polygons)
    return builder.tree

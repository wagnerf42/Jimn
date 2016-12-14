"""
implementation of betley ottmann intersection algorithm.
"""
from collections import defaultdict
from sortedcontainers import SortedList
from jimn.elementary_path import set_comparer
from jimn.tree.inclusion_tree import InclusionTree
from jimn.tree.inclusion_tree.polygonsegment import polygon_segments


class InclusionTreeBuilder:
    """
    this class builds a tree of polygons included in one another.
    it works through a sweeping line algorithm.
    also identifies each as a hole or a polygon.
    """
    def __init__(self, polygons):
        # the algorithm works in O(n) (times sorted container's costs) in this way:
        # we have a SortedList of all currently crossed paths
        self.crossed_paths = SortedList()
        # for each polygon, a SortedList of all of its currently crossed paths
        self.polygons = defaultdict(SortedList)
        # when meeting a new polygon for the first time
        # we will insert it in the crossed_paths list ; we get it's top neighbour (smaller)
        # and get the corresponding polygon
        # now if we are contained inside it, we are its child
        # if we are not contained inside it, we are its brother
        # to figure out whether we are inside or not, we look at #paths smaller than us
        # in the nieghbour polygon's SortedList

        set_comparer(self)
        # we store all keys used for comparing paths
        # this speeds up keys computations and more importantly removes
        # rounding errors
        self.sweeping_keys = dict()

        self._create_events(polygons)
        self.current_point = None
        self.tree = InclusionTree()
        self.identified_polygons = set()

        for event in self.events:
            self.execute_event(event)
            if len(self.identified_polygons) == len(polygons):
                return  # no need to finish the sweep once everyone is identified

    def _create_events(self, polygons):
        """
        create all start/end events for each path.
        each event is : a comparison key ; the path.
        """
        self.events = []
        for height, polygons in polygons.items():
            for polygon in polygons:
                for segment in polygon_segments(height, polygon):
                    angle = segment.angle()
                    for point in sorted(segment.endpoints):
                        key = (point, angle, -height)
                        self.events.append((key, segment))
                        self.sweeping_keys[(id(segment), point)] = key
                        angle *= -1

        self.events.sort(key=lambda e: e[0])

    def key(self, path):
        """
        returns key at current point for given path.
        """
        key_id = (id(path), self.current_point)
        if key_id in self.sweeping_keys:
            return self.sweeping_keys[key_id]
        else:
            current_x = self.current_point.coordinates[0]
            return (path.vertical_intersection_at(current_x),
                    path.key_angle(),
                    path.height)

    def execute_event(self, event):
        """
        execute start path or end path event
        """
        event_key, event_path = event
        event_point, event_angle = event_key[0:2]

        if event_angle >= 0:
            # start event
            self.current_point = event_point
            self.start_path(event_path)
        else:
            # end event
            self.end_path(event_path)
            self.current_point = event_point

    def start_path(self, path):
        """
        handles incoming path
        """
        self.crossed_paths.add(path)
        polygon = path.polygon_id()
        self.polygons[polygon].add(path)
        if polygon not in self.identified_polygons:
            self.identified_polygons.add(polygon)
            raise Exception("TODO")

    def end_path(self, path):
        """
        handles ending path
        """
        self.crossed_paths.remove(path)
        self.polygons[path.polygon_id()].remove(path)


def build_inclusion_tree(polygons):
    """
    turn a set of polygons hashed by height into a polygon tree.
    """
    builder = InclusionTreeBuilder(polygons)
    return builder.tree

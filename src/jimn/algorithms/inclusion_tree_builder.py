"""
implementation of betley ottmann intersection algorithm.
"""
from collections import defaultdict
from sortedcontainers import SortedList
from jimn.elementary_path import set_comparer
from jimn.tree.inclusion_tree import InclusionTree
from jimn.tree.inclusion_tree.polygonsegment import polygon_segments
from jimn.utils.debug import is_module_debugged
from jimn.displayable import tycat

END_EVENT = 0
START_EVENT = 1

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

        polygons_number = self._create_events(polygons)
        self.current_point = None
        self.tree = InclusionTree()
        self.nodes = dict()  # store for each poly its node and father node

        for event in self.events:
            self.execute_event(event)
            if len(self.nodes) == polygons_number:
                return  # no need to finish the sweep once everyone is identified

    def _create_events(self, polygons):
        """
        create all start/end events for each path.
        each event is : a comparison key ; the path.
        """
        self.events = []
        polygons_number = 0
        for height, polygons in polygons.items():
            for polygon in polygons:
                polygons_number += 1
                for segment in polygon_segments(height, polygon):
                    angle = segment.key_angle()
                    print("angle for", segment, "is", angle)
                    for point, event_type in zip(
                            sorted(segment.endpoints), (START_EVENT, END_EVENT)):
                        key = (point, event_type, -height)
                        self.events.append((key, segment))
                        self.sweeping_keys[(id(segment), point)] =\
                            (point.coordinates[1], angle, -height)

        self.events.sort(key=lambda e: e[0])
        return polygons_number

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
                    -path.height)

    def execute_event(self, event):
        """
        execute start path or end path event
        """
        event_key, event_path = event
        event_point, event_type = event_key[0:2]

        if event_type == START_EVENT:
            self.current_point = event_point
            self.start_path(event_path)
        else:
            self.end_path(event_path)
            self.current_point = event_point

        if __debug__:
            # very slow
            paths = iter(self.crossed_paths)
            previous_path = next(paths, None)
            for path in paths:
                if self.key(previous_path) >= self.key(path):
                    paths = list(self.crossed_paths)
                    print(paths)
                    print("previous", previous_path, self.key(previous_path))
                    print("current", path, self.key(path))
                    tycat(self.current_point, paths, previous_path, path)
                    raise Exception("pb ordre")
                previous_path = path

    def start_path(self, path):
        """
        handles incoming path
        """
        index = self.crossed_paths.bisect(path)
        self.crossed_paths.insert(index, path)
        polygon = path.polygon_id()
        self.polygons[polygon].add(path)

        if polygon not in self.nodes:
            father_node = self.identify_father_node(path, index)
            new_node = father_node.add_child(path)
            self.nodes[polygon] = (new_node, father_node)
            print("adding", polygon, "as child of", id(father_node.content))

    def identify_father_node(self, path, index):
        """
        identify where polygon is in tree.
        we need the path and its position in crossed paths
        """
        if index == 0:
            # no one above us, we are below root
            return self.tree
        else:
            neighbour_polygon = self.crossed_paths[index-1].polygon_id()
            above_paths = self.polygons[neighbour_polygon].bisect(path)
            if above_paths % 2:
                # odd neighbour's paths above us
                # we are inside him
                return self.nodes[neighbour_polygon][0]
            else:
                # event neighbour's paths above us
                # we are beside him
                return self.nodes[neighbour_polygon][1]

    def end_path(self, path):
        """
        handles ending path
        """
        print("removing", path, "from", self.crossed_paths)
        self.crossed_paths.remove(path)
        self.polygons[path.polygon_id()].remove(path)


def build_inclusion_tree(polygons):
    """
    turn a set of polygons hashed by height into a polygon tree.
    """
    builder = InclusionTreeBuilder(polygons)
    if __debug__:
        if is_module_debugged(__name__):
            print("inclusion tree")
            builder.tree.tycat()
    return builder.tree

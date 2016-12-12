"""
implementation of betley ottmann intersection algorithm.
"""
from collections import defaultdict
from sortedcontainers import SortedSet, SortedList
from jimn.utils.debug import is_module_debugged
from jimn.tree.treap import Treap
from jimn.point import Point
from jimn.displayable import tycat
from jimn.bounding_box import BoundingBox
from jimn.utils.coordinates_hash import ROUNDER2D
from jimn.utils.iterators import triplets
from jimn.segment import Segment
from jimn.elementary_path import set_comparer

class Cutter:
    """
    state object for executing bentley ottmann intersection algorithm.
    this algorithm is generalized to accept both segments and arcs.
    """
    def __init__(self, paths):
        self.paths = paths  # we store paths for debugging purposes
        self.events = SortedSet()  # all events go here for easy access

        # we store all keys used for comparing paths
        # this speeds up keys computations and more importantly removes
        # rounding errors
        self.sweeping_keys = dict()

        # we store results : associate to each path a list of intersections
        self.intersections = defaultdict(list)

        # how to react at a given event point ?
        # -> we remember what path (list) start here, end here
        self.events_data = (defaultdict(set), defaultdict(set))

        # set ourselves as comparison tool for paths
        set_comparer(self)

        # we store paths cut by current vertical line
        self.crossed_paths = SortedList()

        # create all start/end events
        for path in self.paths:
            for point, path_storage, angle_multiplier in \
                    zip(sorted(path.endpoints), self.events_data, (1, -1)):
                self.events.add(point)
                path_storage[point].add(path)
                self.add_key(path, point, angle_multiplier)

        self.current_point = None

        if __debug__:
            if is_module_debugged(__name__):
                print("starting bentley ottmann")
                tycat(self.paths)

    def add_key(self, path, point, angle_multiplier=1):
        """
        register a key.
        angle multiplier for inverting angles at end of path.
        """
        new_key = (point.coordinates[1], path.key_angle()*angle_multiplier)
        self.sweeping_keys[(id(path), point)] = new_key
        return new_key

    def key(self, path):
        """
        returns key at current point for given path.
        """
        key_id = (id(path), self.current_point)
        if key_id in self.sweeping_keys:
            return self.sweeping_keys[key_id]
        else:
            current_x = self.current_point.coordinates[0]
            return (path.vertical_intersection_at(current_x), path.key_angle())

    def add_path(self, path):
        """
        handles incoming path.
        add to crossed path and check for neighbouring intersections.
        """
        self.crossed_paths.add(path)
        new_index = self.crossed_paths.index(path)
        for index in (new_index-1, new_index+1):
            if 0 < index < len(self.crossed_paths)-1:
                neighbour_path = self.crossed_paths[index]
                intersections = path.intersections_with(neighbour_path)
                for intersection in intersections:
                    self.add_intersection(intersection, (path, neighbour_path))

    def remove_paths(self, ending_paths):
        """
        remove some paths and check for neighbouring intersections.
        """
        def contiguous_ranges(indices):
            """
            iterate on all starts-1 and ends+1 of contiguous ranges of numbers
            given by iterator
            """
            current_index = next(indices)
            start = current_index-1
            for index in indices:
                if index > current_index+1:
                    yield start, current_index+1
                    start = index-1
                current_index = index
            yield start, current_index+1

        indices = [self.crossed_paths.index(p) for p in ending_paths]
        paths = []
        for neighbour_indices in contiguous_ranges(iter(indices)):
            if neighbour_indices[0] >= 0 and neighbour_indices[1] < len(self.crossed_paths):
                paths = [self.crossed_paths[i] for i in neighbour_indices]
                intersections = paths[0].intersections_with(paths[1])
                for intersection in intersections:
                    self.add_intersection(intersection, paths)

        #now, remove everyone
        for index in sorted(indices, reverse=True):
            del self.crossed_paths[index]

    def add_intersection(self, intersection, intersecting_paths):
        """
        store intersection, prepare for nodes swap
        """
        if intersection <= self.current_point:
            return
        self.events.add(intersection)
        for path in intersecting_paths:

            # we immediately register comparison key at this intersection
            self.add_key(path, intersection)

            if intersection != path.endpoints[0] and intersection != path.endpoints[1]:
                self.events_data[1][intersection].add(path)  # path will end
                self.events_data[0][intersection].add(path)  # and restart
                self.intersections[id(path)].append(intersection)

    def execute(self):
        """
        run bentley ottmann
        """
        while self.events:
            event_point = self.events.pop(0)

            # remove ending paths
            self.remove_paths(self.events_data[1][event_point])

            self.current_point = event_point
            if __debug__:
                if is_module_debugged(__name__):
                    print("current point is now", self.current_point)

            # add starting paths
            for starting_path in self.events_data[0][event_point]:
                self.add_path(starting_path)

            if __debug__:
                if is_module_debugged(__name__):
                    self.tycat()

        return self

    def tycat(self):
        """
        graphical display for debugging
        """
        # compute intersections
        intersections = []
        for small_intersections in self.intersections.values():
            intersections.extend(small_intersections)
        intersections = list(set(intersections))

        # compute current vertical line
        bbox = BoundingBox.empty_box(2)
        for path in self.paths:
            bbox.update(path.get_bounding_box())
        ymin, ymax = bbox.limits(1)
        height = ymax - ymin
        line_y_min = ymin - height/20
        line_y_max = ymax + height/20
        current_x = self.current_point.coordinates[0]
        vertical_line = Segment([
            Point([current_x, line_y_min]),
            Point([current_x, line_y_max])
        ])

        # display figure
        tycat(self.paths, intersections, [self.current_point, vertical_line],
              *self.crossed_paths)

        print(list(self.key(p) for p in self.crossed_paths))


def compute_intersections(paths):
    """
    slice given paths into elementary paths
    """
    #tycat(paths, Cutter(paths).execute())
    cutter = Cutter(paths).execute()
    #TODO
    return list(cutter.intersections.values())

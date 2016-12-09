"""
implementation of betley ottmann intersection algorithm.
"""
from collections import defaultdict
from sortedcontainers import SortedSet
from jimn.utils.debug import is_module_debugged
from jimn.tree.treap import Treap
from jimn.point import Point
from jimn.displayable import tycat
from jimn.bounding_box import BoundingBox
from jimn.segment import Segment


class Cutter:
    """
    state object for executing bentley ottmann intersection algorithm.
    this algorithm is generalized to accept both segments and arcs.
    """
    def __init__(self, paths):
        self.paths = paths  # we store paths for debugging purposes
        self.events = SortedSet()  # all events go here for easy access

        # we store results : associate to each path a list of intersections
        self.intersections = defaultdict(list)

        # how to react at a given event point ?
        # -> we remember what path (list) start here, end here
        self.events_data = (defaultdict(set), defaultdict(set))

        # we store paths cut by current vertical line
        sentinel = Segment([Point([-10000, 10000]), Point([10000, 10000])])
        self.crossed_paths = Treap(sentinel, root_node=True)
        self.crossed_paths.set_comparer(self)

        # create all start/end events
        for path in self.paths:
            for point, path_storage in zip(sorted(path.endpoints), self.events_data):
                self.events.add(point)
                path_storage[point].add(path)

        self.current_point = None

        if __debug__:
            if is_module_debugged(__name__):
                print("starting bentley ottmann")
                tycat(self.paths)

    def key(self, path):
        """
        returns key at current point for given path.
        """
        return path.sweeping_key(self.current_point)

    def add_path(self, path):
        """
        handles incoming path.
        add to crossed path and check for neighbouring intersections.
        """
        node = self.crossed_paths.add(path)
        for neighbour in node.neighbours():
            neighbour_path = neighbour.content
            intersections = path.intersections_with(neighbour_path)
            for intersection in intersections:
                self.add_intersection(intersection, (path, neighbour_path))

    def remove_paths(self, ending_paths):
        """
        remove some paths and check for neighbouring intersections.
        """
        #TODO: optimize by only comparing top neighbour with bottom one
        for ending_path in ending_paths:
            node = self.crossed_paths.find(ending_path)
            neighbours = node.neighbours()
            node.remove()
            if len(neighbours) == 2:
                paths = [n.content for n in neighbours]
                intersections = paths[0].intersections_with(paths[1])
                for intersection in intersections:
                    self.add_intersection(intersection, paths)

    def add_intersection(self, intersection, intersecting_paths):
        """
        store intersection, prepare for nodes swap
        """
        if intersection <= self.current_point:
            return
        self.events.add(intersection)
        for path in intersecting_paths:
            if intersection != path.endpoints[0] and intersection != path.endpoints[1]:
                self.events_data[1][intersection].add(path)  # path will end
                self.events_data[0][intersection].add(path)  # and restart
                self.intersections[id(path)].append(intersection)

    def execute(self):
        """
        run bentley ottmann and gives back intersections
        """
        while self.events:
            event_point = self.events.pop(0)

            # remove ending paths
            self.remove_paths(self.events_data[1][event_point])

            self.current_point = event_point

            # add starting paths
            for starting_path in self.events_data[0][event_point]:
                self.add_path(starting_path)
            # self.tycat()
        return []

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
              *self.crossed_paths.ordered_contents())
        # display treap
        self.crossed_paths.tycat()


def compute_intersections(paths):
    """
    slice given paths into elementary paths
    """
    #tycat(paths, Cutter(paths).execute())
    Cutter(paths).execute()
    #raise Exception("TODO")
    return []

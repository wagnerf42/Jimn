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
        self.intersections = dict()

        # how to react at a given event point ?
        # -> we remember what path (list) start here, end here and which nodes will be swapped
        self.events_data = (defaultdict(list), defaultdict(list), defaultdict(set))

        # we store paths cut by current vertical line
        sentinel = Segment([Point([-10000, 10000]), Point([10000, 10000])])
        self.crossed_paths = Treap(sentinel, root_node=True)
        self.crossed_paths.set_comparer(self)

        # create all start/end events
        for path in self.paths:
            for point, path_storage in zip(path.endpoints, self.events_data):
                self.events.add(point)
                path_storage[point].append(path)

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
            intersection = path.intersection_with(neighbour_path)
            if intersection:
                self.add_intersection(intersection, [node, neighbour])

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
                intersection = neighbours[0].content.intersection_with(neighbours[1].content)
                if intersection:
                    self.add_intersection(intersection, neighbours)

    def add_intersection(self, intersection, nodes):
        """
        store intersection, prepare for nodes swap
        """
        assert intersection > self.current_point
        self.events.add(intersection)
        for node in nodes:
            path = node.content
            if intersection != path.endpoints[0] and intersection != path.endpoints[1]:
                self.events_data[2][intersection].add(node)
                self.intersections[id(path)].append(intersection)

    def swap_nodes(self, point):
        """
        remove and re-insert nodes intersecting at given point
        """
        nodes = self.events_data[2]
        for node in nodes:
            node.remove()

        self.current_point = point

        for node in nodes:
            self.crossed_paths.add(node.content)

    def execute(self):
        """
        run bentley ottmann and gives back intersections
        """
        while self.events:
            event_point = self.events.pop(0)

            # remove ending paths
            self.remove_paths(self.events_data[1][event_point])

            # swap nodes
            self.swap_nodes(event_point)

            # add starting paths
            for starting_path in self.events_data[0][event_point]:
                self.add_path(starting_path)
            self.tycat()
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
        tycat(self.paths, [self.current_point, vertical_line],
              intersections, *self.crossed_paths.ordered_contents())
        # display treap
        self.crossed_paths.tycat()


def compute_intersections(paths):
    """
    slice given paths into elementary paths
    """
    tycat(paths, Cutter(paths).execute())
    raise Exception("TODO")

"""
main module for sweeping line algorithms.

provides a 'SweepingLineAlgorithm' class to derive from.
factorizes common operations between different algorithms.
"""
from collections import defaultdict
from math import pi
from heapq import heappush, heappop
from jimn.point import Point
from jimn.segment import Segment
from jimn.arc import Arc
from jimn.tree.treap import Treap
from jimn.utils.debug import is_module_debugged
from jimn.displayable import tycat
from jimn.elementary_path import ElementaryPath


class SweepingLineAlgorithm:
    """
    base class for sweeping line algorithms.
    it should not be used as itself but derived from.

    declare your new algorithm class:

        class MySweepAlgorithm(SweepingLineAlgorithm):
            def __init__(self, ...):
                ...
            super().__init__( ARRAY OF ALL PATHS TO SWEEP )

    you then need to provide the following handlers as methods of your class:

        def add_paths(self, paths):
            ... add a set of paths to set of current paths ...

        def remove_paths(self, path):
            ... remove a set of paths from set of current paths ...

    events can be added to the system by calling
        add_path_events (adding both start and end)
        or
        add_end_event (adding only end event)
    """
    def __init__(self, paths, cut_arcs=False):
        """
        prepare for sweeping line algorithm on a set of paths.
        """
        # sweeping line algorithms are based on events
        # each event is meeting a new point
        self.events = []  # store them all in a heap
        self.seen_points = set()  # do not add twice events for same point

        # all starting and ending paths
        self.paths = [defaultdict(list), defaultdict(list)]

        if cut_arcs:
            self._add_cut_arcs(paths)
        else:
            for path in paths:
                self.add_path_events(path, sorted(path.endpoints))

        self.current_point = None  # current point in sweeping movement
        # (with respect to content of crossed paths)
        self.incoming_point = None  # position in sweeping movement

        # visible paths at current_point
        # we put a sentinel as root node whose key values at +infinity
        self.crossed_paths = Treap((float("+inf"), 0), root_node=True)
        self.crossed_paths.set_comparer(self)

        self._run()

    def _add_cut_arcs(self, paths):
        """
        insert given paths events.
        arcs are cut into smaller pieces preventing vertical overlaps.
        """
        for path in paths:
            if isinstance(path, Arc):
                arcs = path.horizontal_split()
                for arc in arcs:
                    self.add_path_events(arc, sorted(arc.endpoints))
            else:
                self.add_path_events(path, sorted(path.endpoints))

    def key(self, path):
        """
        return comparison key for given path, at current point.
        (key is: current_y ; outgoing angle ; terminal angle)
        pre-condition: path contains current point's x coordinate in its range.
        """
        if not isinstance(path, ElementaryPath):
            return path  # special case for sentinel : no need to compute

        current_x = self.current_point.get_x()
        if __debug__:
            x_coordinates = sorted([p.get_x() for p in path.endpoints])
            if not x_coordinates[0] <= current_x <= x_coordinates[1]:
                print("path is", path, "current point is:", self.current_point)
                raise Exception("non comparable path in tree")

        # start by finding the path's y for current x
        point_key = Point([current_x,
                           path.vertical_intersection_at(current_x)])

        if path.endpoints[0] < path.endpoints[1]:
            first_point, last_point = path.endpoints
        else:
            last_point, first_point = path.endpoints

        terminal_angle = (pi/2 - first_point.angle_with(last_point)) % pi

        if isinstance(path, Segment):
            angles = (terminal_angle, terminal_angle)
        else:
            outgoing_angle = (pi - path.center.angle_with(point_key)) % pi
            angles = (outgoing_angle, terminal_angle)

        # now just reverse angles based on direction
        if last_point > point_key:
            full_key = (point_key.get_y(), angles[0], angles[1])
        else:
            full_key = (point_key.get_y(), -angles[0], -angles[1])
        return full_key

    def add_path_events(self, path, points):
        """
        at start and end events for given path at given points.
        """
        for index, point in enumerate(points):
            if point not in self.seen_points:
                self.seen_points.add(point)
                heappush(self.events, point)

            self.paths[index][point].append(path)

    def add_end_event(self, path, end_point):
        """
        add end event for given path.
        """
        if end_point not in self.seen_points:
            self.seen_points.add(end_point)
            heappush(self.events, end_point)
        self.paths[1][end_point].append(path)

    def tycat(self):
        """
        display current state.
        """
        tycat(self.current_point, *self.crossed_paths.ordered_contents())
        self.crossed_paths.tycat()

    def _run(self):
        while self.events:
            event_point = heappop(self.events)
            event_paths = []
            for paths in self.paths:
                event_paths.append(paths[event_point])
                del paths[event_point]
            self._handle_events(event_point, event_paths[0], event_paths[1])

    def _handle_events(self, event_point, starting_paths, ending_paths):
        # pylint: disable=no-member
        if __debug__:
            if self.current_point is not None:
                if self.current_point > event_point:
                    print("faulty point :", event_point,
                          "current point :", self.current_point)
                    raise Exception("going back")
        self.incoming_point = event_point
        try:
            self.remove_paths(ending_paths)
        except:
            print("failed removing paths at", event_point)
            raise

        self.current_point = event_point
        # remove current point from seen points
        # to allow seeing it again in case of intersections
        self.seen_points.discard(self.current_point)

        sorted_paths = sorted(starting_paths, key=self.key)
        self.add_paths(sorted_paths)

        if __debug__:
            if is_module_debugged(__name__):
                print("current point =", self.current_point)
                self.tycat()


def key_terminal_angle(points):
    """
    angle of line going through points and vertical line
    """
    if points[0] > points[1]:
        return (pi/2 - points[1].angle_with(points[0])) % pi
    else:
        return (pi/2 - points[0].angle_with(points[1])) % pi


def key_outgoing_angle(arc, tangent_point):
    """
    angle of tangent line of arc at tangent point and vertical line
    """
    return (pi - arc.center.angle_with(tangent_point)) % pi

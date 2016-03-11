"""
main class for sweeping line algorithms.
"""
from collections import defaultdict
from math import pi
from heapq import heappush, heappop
from jimn.point import Point
from jimn.segment import Segment
from jimn.arc import Arc
from jimn.tree.treap import Treap
from jimn.utils.coordinates_hash import ROUNDER2D
from jimn.utils.debug import is_module_debugged
from jimn.displayable import tycat


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

        def handle_vertical_paths(self, path):
            ... take care of all vertical paths at current x ...

    events can be added to the system by calling
        add_path_events (adding both start and end)
        or
        add_end_event (adding only end event)
    """
    def __init__(self, paths):
        """
        prepare for sweeping line algorithm on a set of paths.
        """
        # sweeping line algorithms are based on events
        # each event is meeting a new x coordinate in the figure
        self.events = []  # store them all in a heap
        self.seen_coordinates = set()  # do not add twice events for same x

        # all starting and ending paths
        self.paths = [defaultdict(list), defaultdict(list)]

        for path in paths:
            path_x_coordinates = list(
                sorted([p.get_x() for p in path.endpoints]))
            self.add_path_events(path, path_x_coordinates)

        self.current_x = None  # current x coordinate in sweeping movement
        # TODO: use min and max dimensions

        # visible paths at current_x
        self.crossed_paths = Treap(
            Segment([Point([-2000, 2000]), Point([2000, 2000])]),
            root_node=True
        )
        self.crossed_paths.set_comparer(self)
        self._run()

    def key(self, path):
        """
        return comparison key for given path, at current point.
        """
        return comparison_key(path, self.current_x)

    def add_path_events(self, path, coordinates):
        """
        at start and end events for given path at given points.
        """
        if coordinates[0] == coordinates[1]:
            print("discarding vertical path")
            return

        for index, point_x in enumerate(coordinates):
            if point_x not in self.seen_coordinates:
                self.seen_coordinates.add(point_x)
                heappush(self.events, point_x)

            self.paths[index][point_x].append(path)

    def add_end_event(self, path, end_coordinate):
        """
        add end event for given path.
        """
        if end_coordinate not in self.seen_coordinates:
            self.seen_coordinates.add(end_coordinate)
            heappush(self.events, end_coordinate)
        self.paths[1][end_coordinate].append(path)

    def _run(self):
        while self.events:
            event_x = heappop(self.events)
            self._handle_events(event_x, self.paths[0][event_x],
                                self.paths[1][event_x])

    def _handle_events(self, event_x, starting_paths, ending_paths):
        # pylint: disable=no-member
        if __debug__:
            if self.current_x is not None:
                assert(self.current_x < event_x), "event going back"
        try:
            self.remove_paths(ending_paths)
        except:
            print("failed removing paths at", event_x)
            raise

        self.current_x = event_x

        sorted_paths = sorted(starting_paths,
                              key=lambda p: p.comparison_key(
                                  self.current_x))

        self.add_paths(sorted_paths)
        if __debug__:
            if is_module_debugged(__name__):
                print("x=", self.current_x)
                tycat(*self.crossed_paths.ordered_contents())


def comparison_key(path, current_x):
    """
    return key used for comparing paths when reaching given point in
    sweeping line algorithm.
    (key is: current_y ; outgoing angle)
    pre-condition: self contains point's x coordinate in its xrange
    """
    # start by finding the path's y for current x
    if __debug__:
        x_coordinates = sorted([p.get_x() for p in path.endpoints])
        assert (x_coordinates[0] <= current_x <= x_coordinates[1]), \
            "non comparable path in tree"
    point_key = Point([current_x, path.vertical_intersection_at(current_x)])
    point_key = ROUNDER2D.hash_point(point_key)

    # now figure out which direction we leave the point
    if isinstance(path, Segment):
        forward_point = max(path.endpoints)
    else:
        tangent_points = path.tangent_points(point_key)
        oriented_points = list(sorted(path.endpoints))
        direction = oriented_points[1] - oriented_points[0]
        if direction.scalar_product(tangent_points[0]-point_key) > 0:
            forward_point = tangent_points[0]
        else:
            forward_point = tangent_points[1]

    # compute and convert angle from horizontal to vertical
    angle_key = (5 * pi/2 - point_key.angle_with(forward_point)) % (2*pi)
    return (point_key.get_y(), angle_key)


setattr(Segment, "comparison_key", comparison_key)
setattr(Arc, "comparison_key", comparison_key)

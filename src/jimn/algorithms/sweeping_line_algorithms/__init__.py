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

START = 0
END = 1


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

        def add_path(self, path):
            ... add a path to set of current paths ...

        def remove_path(self, path):
            ... removes a path from set of current paths ...

        def crossing_paths(self, paths):
            ... a set of paths cross at a common point ...

    any operation generating new events (for example crossing events)
    must add new events using "add_event" method.
    """
    # pylint: disable=too-few-public-methods
    def __init__(self, paths):
        """
        prepare for sweeping line algorithm on a set of paths
        preconditions:
        - paths should not intersect other than on endpoints
        - there is no orientation condition on paths
        """
        # sweeping line algorithms are based on events
        # each event is meeting a new point in the figure
        self.event_points = []  # store them all in a heap

        # now we need additional info associated to each event point
        self.crossings = dict()  # associate to each crossing point the paths
        # all starting and ending paths
        self.paths = [defaultdict(list), defaultdict(list)]

        self._create_events(paths)
        self.current_point = None  # current point in sweeping movement
        # TODO: use min and max dimensions
        self.crossed_paths = Treap(
            Segment([Point([-2000, 2000]), Point([2000, 2000])]),
            root_node=True
        )
        self._run()

    def add_crossing_event(self, crossing_point, crossing_paths):
        """
        add crossing event in system.
        should come after current event.
        """
        if crossing_point in self.crossings:
            for path in crossing_paths:
                self.crossings[crossing_point].add(path)
        else:
            self.crossings[crossing_point] = set(crossing_paths)
            heappush(self.event_points, crossing_point)

    def _create_events(self, paths):
        # create all events
        points = set()
        for path in paths:
            for start_or_end, point in enumerate(list(sorted(path.endpoints))):
                self.paths[start_or_end][point].append(path)
                points.add(point)

        # we now build events heap
        for point in points:
            heappush(self.event_points, point)

    def _run(self):
        while self.event_points:
            event_point = heappop(self.event_points)
            self._handle_event(event_point)

    def _handle_event(self, event_point):
        ending_paths = self.paths[END][event_point]
        if event_point in self.crossings:
            ending_paths.extend(self.crossings[event_point])

        for path in ending_paths:
            self.remove_path(path)

        self.current_point = event_point

        starting_paths = self.paths[START][event_point]
        if event_point in self.crossings:
            starting_paths.extend(self.crossings[event_point])

        for path in sorted(starting_paths,
                           key=lambda p: p.comparison_key(self.current_point)):
            self.add_path(path)


def comparison_key(path, point):
    """
    return key used for comparing paths when reaching given point in
    sweeping line algorithm.
    pre-condition: self contains point's x coordinate in its xrange
    """
    if path.contains(point):
        point_key = point
    else:
        point_key = Point([point.get_x(),
                           path.vertical_intersection_at(point.get_x())])

    if isinstance(path, Segment):
        forward_point = max(path.endpoints)
    else:
        tangent_points = path.tangent_points(point)
        oriented_points = list(sorted(path.endpoints))
        direction = oriented_points[1] - oriented_points[0]
        if direction.scalar_product(tangent_points[0]-point) > 0:
            forward_point = tangent_points[0]
        else:
            forward_point = tangent_points[1]

    # compute and convert angle from horizontal to vertical
    angle_key = (5 * pi/2 - point.angle_with(forward_point)) % (2*pi)
    return (point_key, angle_key)


setattr(Segment, "comparison_key", comparison_key)
setattr(Arc, "comparison_key", comparison_key)

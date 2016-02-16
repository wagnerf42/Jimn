"""
main class for sweeping line algorithms.
"""
from heapq import heappush, heappop
from jimn.algorithms.sweeping_line_algorithms.event import Event


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
        self.paths = paths
        self.events = []  # events heap
        self.crossings = dict() # associate to each crossing point the paths
        self._create_events()
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

        heappush(self.events, Event(crossing_point))

    def _create_events(self):
        # create all events
        events = {}
        for path in self.paths:
            start, end = list(sorted(path.endpoints))
            for path_type, extremity in enumerate([start, end]):
                if extremity not in events:
                    events[extremity] = Event(extremity)
                events[extremity].add_path(path_type, path)
        # we sort the events in lexicographical order
        # to prepare for sweeping algorithm
        for event in events.values():
            heappush(self.events, event)

    def _run(self):
        while self.events:
            event = heappop(self.events)
            self._handle_event(event)

    def _handle_event(self, event):
        # pylint: disable=no-member
        starting_paths, ending_paths = [
            event.paths[path_type] for path_type in (0, 1)
        ]
        for path in ending_paths:
            self.remove_path(path)

        if event.event_point in self.crossings:
            self.crossing_paths(self.crossings[event.event_point])

        # TODO: angle here is not ok :-(
        for segment in sorted(starting_paths,
                              key=lambda seg: (seg.angle(), seg.height),
                              reverse=True):
            self.add_path(segment)

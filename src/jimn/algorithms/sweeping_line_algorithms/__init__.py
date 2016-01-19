"""
main class for sweeping line algorithms.
it should not be used as itself but derived from.
you need to overload 'terminate_polygon' and
'handle_new_paths'
"""
from collections import defaultdict
from jimn.algorithms.sweeping_line_algorithms.event import Event


class SweepingLineAlgorithm:
    """
    base class for sweeping line algorithms.
    it should not be used as itself but derived from.
    you need to overload 'terminate_polygon' and
    'handle_new_paths'.
    """
    def __init__(self, paths):
        """
        prepare for sweeping line algorithm on a set of paths
        preconditions:
        - paths should not intersect other than on endpoints
        - there is no orientation condition on paths
        """
        self.paths = paths
        # paths currently crossed by sweeping line
        self.current_paths = defaultdict(list)
        self._create_events()
        self._run()

    def terminate_polygon(self, polygon_id):
        """handler called when encountering last path of a polygon"""
        return

    def handle_new_paths(self, starting_paths):
        """handler called when starting new paths at some point"""
        return

    def _create_events(self):
        # create all events
        events = {}
        for path in self.paths:
            start, end = list(sorted(path.get_endpoints()))
            for path_type, extremity in enumerate([start, end]):
                if extremity not in events:
                    events[extremity] = Event(extremity)
                events[extremity].add_path(path_type, path)
        # we sort the events in lexicographical order
        # to prepare for sweeping algorithm
        self.events = sorted(events.values())

    def _run(self):
        for event in self.events:
            self._handle_event(event)

    def _handle_event(self, event):
        # update live paths
        starting_paths, ending_paths = [
            event.paths[path_type] for path_type in [0, 1]
        ]
        self._update_live_paths(starting_paths, ending_paths)
        self.handle_new_paths(starting_paths)

    def _update_live_paths(self, starting_paths, ending_paths):
        for path in ending_paths:
            self._remove_path(path)
        for path in starting_paths:
            self._add_path(path)

    def _add_path(self, path):
        polygon_id = path.get_polygon_id()
        self.current_paths[polygon_id].append(path)

    def _remove_path(self, path):
        polygon_id = path.get_polygon_id()
        self.current_paths[polygon_id].remove(path)
        if not self.current_paths[polygon_id]:
            del self.current_paths[polygon_id]
            self.terminate_polygon(polygon_id)

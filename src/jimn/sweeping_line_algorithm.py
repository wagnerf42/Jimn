from jimn.event import event
from collections import defaultdict

"""
main class for sweeping line algorithms.
it should not be used as itself but derived from.
you need to overload 'terminate_polygon' and
'handle_new_paths'
"""


class sweeping_line_algorithm:
    def __init__(self, paths):
        """
        prepare for sweeping line algorithm on a set of paths
        preconditions:
        - paths should not intersect other than on endpoints
        - there is no orientation condition on paths
        """
        self.paths = paths
        self.current_paths = defaultdict(list)  # currently crossed by sweeping line
        self._create_events()
        self._run()

    def terminate_polygon(self, polygon_id):
        """handler called when encountering last path of a polygon"""
        return

    def _create_events(self):
        # create all events
        events = {}
        for p in self.paths:
            start, end = p.sort_endpoints().get_endpoints()
            for path_type, extremity in enumerate([start, end]):
                if extremity not in events:
                    events[extremity] = event(extremity)
                events[extremity].add_path(path_type, p)
        # we sort the events in lexicographical order
        # to prepare for sweeping algorithm
        self.events = sorted(events.values())

    def _run(self):
        for e in self.events:
            self._handle_event(e)

    def _handle_event(self, e):
        # update live paths
        starting_paths, ending_paths = [
            e.get_paths(path_type) for path_type in [0, 1]
        ]
        self._update_live_paths(starting_paths, ending_paths)
        self.handle_new_paths(starting_paths)

    def _update_live_paths(self, starting_paths, ending_paths):
        for p in ending_paths:
            self._remove_path(p)
        for p in starting_paths:
            self._add_path(p)

    def _add_path(self, p):
        polygon_id = p.get_polygon_id()
        self.current_paths[polygon_id].append(p)

    def _remove_path(self, p):
        polygon_id = p.get_polygon_id()
        self.current_paths[polygon_id].remove(p)
        if not self.current_paths[polygon_id]:
            del self.current_paths[polygon_id]
            self.terminate_polygon(polygon_id)

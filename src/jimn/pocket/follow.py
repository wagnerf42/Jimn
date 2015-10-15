"""
build subpockets by following paths
"""


class follower:
    def __init__(self, split_pocket):
        self.pocket = split_pocket
        # where to go from each point (dynamic)
        self.points = defaultdict(list)
        for p in split_pocket.get_content():
            start = p.get_endpoint(0)
            self.points[start].append(p)

        self.results = []

    def follow(self):
        """
        builds subpockets, following paths.
        """
        while self.points:
            start_point = self.get_starting_point()
            self.seen_points = {}
            self.current_point = start_point
            self.current_path = []
            while True:
                if __debug__:
                    if is_module_debugged(__name__):
                        tycat(self.pocket, self.current_path,
                              self.current_point)
                self.update_path()
                self.move_on()
                if self.current_point == start_point:
                    self.results.append(self.current_path)
                    break

    def update_path(self):
        """
        we reached a new point.
        updates all info.
        """
        if self.current_point in self.seen_points:
            self.results.append(self.remove_current_loop())

        self.seen_points[self.current_point] = len(self.current_path)
        self.current_path.append(self.elementary_path)

    def move_on(self):
        """
        advances on current path.
        """
        self.elementary_path = self.get_next_path()
        assert self.current_point == self.elementary_path.get_endpoint(0)
        self.current_point = self.elementary_path.get_endpoint(1)

    def remove_current_loop(self):
        """
        current point is already in current path.
        removes from current path all part from current point to end.
        """
        i = self.seen_points[self.current_point]
        assert self.current_path[i].get_endpoint(0) == self.current_point
        cycle = self.current_path[i:]
        self.current_path = self.current_path[:i]

        # re hash seen points
        self.seen_points = {}
        for i, p in self.current_path:
            self.seen_points[p.get_endpoint(0)] = i

        if __debug__:
            if is_module_debugged(__name__):
                print("extracting cycle")
                tycat(self.pocket, cycle)

        return cycle

    def get_next_path(self):
        """
        use hashes to quickly find where to go.
        """
        next_path = self.points[self.current_point].pop()
        if not self.points[self.current_point]:
            del self.points[self.current_point]

        return next_path

    def get_starting_point(self):
        """
        returns any valid starting point.
        """
        return next(iter(self.points.keys()))


def split_pocket(p):
    """
    splits pocket p into subpockets obtained by following paths.
    paths in subpockets are ordered by the followed order, so can be tested
    for orientation.
    assumes you can never end in a deadlock.
    assumes only one solution is possible.
    """
    f = follower(p)
    f.follow()
    sub_pockets = [pocket(r) for r in f.results]
    return sub_pockets


from jimn.displayable import tycat
from jimn.pocket import pocket
from jimn.utils.debug import is_module_debugged
from collections import defaultdict

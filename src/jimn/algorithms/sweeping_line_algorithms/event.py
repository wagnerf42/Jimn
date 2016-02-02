"""
event class for sweeping lines algorithms.
"""
START_EVENT = 0
END_EVENT = 1


class Event:
    """
    an event is a point with segments starting and ending there.
    """
    # pylint: disable=too-few-public-methods
    def __init__(self, event_point):
        self.event_point = event_point
        self.paths = [None, None]
        self.paths[START_EVENT] = []
        self.paths[END_EVENT] = []

    def add_path(self, path_type, path):
        """
        add path to event point.
        """
        self.paths[path_type].append(path)

    def __lt__(self, other):
        return self.event_point < other.event_point

    def __str__(self):
        # pylint: disable=not-an-iterable
        string = "event(" + str(self.event_point) + "):\n"
        string += "beg:\n" + " ".join([str(p) for p in self.paths[START_EVENT]])
        string += "\nend:\n" + " ".join([str(p) for p in self.paths[END_EVENT]])
        return string

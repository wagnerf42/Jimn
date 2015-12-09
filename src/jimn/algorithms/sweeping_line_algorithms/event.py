START_EVENT = 0
END_EVENT = 1


class event:
    def __init__(self, event_point):
        self.event_point = event_point
        self.paths = [None, None]
        self.paths[START_EVENT] = []
        self.paths[END_EVENT] = []

    def add_path(self, path_type, p):
        self.paths[path_type].append(p)

    def get_paths(self, path_type):
        return self.paths[path_type]

    def __lt__(a, b):
        return a.event_point < b.event_point

    def __str__(self):
        s = "event(" + str(self.event_point) + "):\n"
        s += "beg:\n" + " ".join([str(p) for p in self.paths[START_EVENT]])
        s += "\nend:\n" + " ".join([str(p) for p in self.paths[END_EVENT]])
        return s

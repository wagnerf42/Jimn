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

    def get_event_point(self):
        return self.event_point

    def __lt__(a, b):
        return a.event_point < b.event_point

    def __str__(self):
        s = "event(" + str(self.event_point) + "):\n"
        s += "beg:\n"
        for p in self.get_paths(0):
            s += str(p) + " "
        s +="\n"
        s += "end:\n"
        for p in self.get_paths(1):
            s += str(p) + " "
        return s
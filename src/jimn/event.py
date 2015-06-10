# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

START_EVENT = 0
END_EVENT = 1


class event:
    def __init__(self, event_point):
        self.event_point = event_point
        self.segments = []
        self.segments[START_EVENT] = []
        self.segments[END_EVENT] = []

    def add_segment(self, segment_type, s):
        self.segments[segment_type].append(s)

    def get_segments(self, segment_type):
        return self.segments[segment_type]

    def __lt__(a, b):
        return a.event_point < b.event_point

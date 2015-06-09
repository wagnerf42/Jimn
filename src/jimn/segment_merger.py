# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.segment import segment
from jimn.coordinates_hash import coordinates_hash
START = 0
END = 1


class segment_merger:
    def __init__(self, segments):
        self.segments = segments
        self.lines = {}
        self.rounder = coordinates_hash()

    def hash_segments(self):
        for s in self.segments:
            h = s.line_hash(self.rounder)
            if h not in self.lines:
                self.lines[h] = []
            self.lines[h].append(s)

    def compute_points_and_counters(self, segments):
        self.points = {}
        self.counters = [{}, {}]
        # loop through each segment
        # we record all points
        # and for each one how many segments start here and end here
        for s in segments:
            assert s.is_sorted(), 'unsorted segment in segment merger'
            endpoints = s.get_endpoints()
            self.points[endpoints[0]] = endpoints[0]
            self.points[endpoints[1]] = endpoints[1]
            for counter, p in zip(self.counters, endpoints):
                if p not in counter:
                    counter[p] = 0
                counter[p] = counter[p] + 1

        # now sort points
        self.sorted_points = sorted(self.points.keys())

    def odd_segments_on_line(self, line_hash):
        segments = self.lines[line_hash]
        self.compute_points_and_counters(segments)
        # now iterate through each point
        # we record on how many segments we currently are
        currently_on = 0
        # we record where interesting segment started
        starting_point = None

        odd_segments = []

        for p in self.sorted_points:
            if p in self.counters[START]:
                now_on = currently_on + self.counters[START][p]
            if p in self.counters[END]:
                now_on = now_on - self.counters[END][p]
            # now test if parity changed
            if (now_on + currently_on) % 2 == 1:
                # we test if we enter or leave a segment
                if now_on % 2 == 1:
                    # enter
                    starting_point = p
                else:
                    # leave
                    odd_segments.append(segment([starting_point, p]))
            currently_on = now_on

        return odd_segments

    def merge(self):
        odd_segments = []
        self.hash_segments()
        for line_hash in self.lines:
            odd_segments.extend(self.odd_segments_on_line(line_hash))
        return odd_segments

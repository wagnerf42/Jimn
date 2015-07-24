from jimn.segment import segment
from jimn.displayable import tycat
from jimn.utils.precision import precision
from jimn.utils.coordinates_hash import coordinates_hash
from collections import defaultdict
START = 0
END = 1


class segment_merger:
    def __init__(self, segments):
        self.segments = segments
        self.lines = defaultdict(list)
        self.rounder = coordinates_hash(2, precision-2)

    def _hash_segments(self):
        """
        hashes aligned segments together
        """
        for s in self.segments:
            h = s.line_hash(self.rounder)
            self.lines[h].append(s)

    def _compute_points_and_counters(self, segments):
        """
        prepares for sweeping through line of aligned segments
        """
        self.points = {}
        self.counters = [defaultdict(int), defaultdict(int)]
        # loop through each segment
        # we record all points
        # and for each one how many segments start here and end here
        for s in segments:
            assert s.is_sorted(), 'unsorted segment in segment merger'
            endpoints = s.get_endpoints()
            self.points[endpoints[0]] = endpoints[0]
            self.points[endpoints[1]] = endpoints[1]
            for counter, p in zip(self.counters, endpoints):
                counter[p] += 1

        # now sort points
        self.sorted_points = sorted(self.points.keys())

    def _odd_segments_on_line(self, line_hash):
        """
        sweeps through line of aligned segments.
        keeping the ones we want
        """
        segments = self.lines[line_hash]
        self._compute_points_and_counters(segments)
        # now iterate through each point
        # we record on how many segments we currently are
        currently_on = 0
        # we record where interesting segment started
        previous_point = None

        odd_segments = []

        for p in self.sorted_points:
            if p in self.counters[START]:
                now_on = currently_on + self.counters[START][p]
            if p in self.counters[END]:
                now_on = now_on - self.counters[END][p]
            if currently_on % 2 == 1:
                odd_segments.append(segment([previous_point, p]))
            previous_point = p
            currently_on = now_on

        return odd_segments

    def merge(self):
        odd_segments = []
        self._hash_segments()
        for line_hash in self.lines:
            kept_segments = self._odd_segments_on_line(line_hash)
            odd_segments.extend(kept_segments)
        return odd_segments


def merge_segments(segments):
    """
    takes a list of potentially overlapping segments.
    returns a list of non overlapping segments.
    overlapping pieces are kept or discarded according to following rule:
        - even number of segments overlap -> discard
        - odd number of segments overlap -> keep
    """
    merger = segment_merger(segments)
    return merger.merge()

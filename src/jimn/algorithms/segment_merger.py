"""
handling overlapping segments.
"""
from collections import defaultdict
from jimn.segment import Segment
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.coordinates_hash import rounder_lines
START = 0
END = 1


class SegmentMerger:
    """
    holds one execution of algorithm merging segments.
    """
    def __init__(self, segments):
        self.segments = segments
        self.points = {}
        self.counters = [defaultdict(int), defaultdict(int)]
        self.lines = defaultdict(list)
        self.sorted_points = None

    def _hash_segments(self):
        """
        hashes aligned segments together
        """
        for segment in self.segments:
            signature = segment.line_hash(rounder_lines)
            self.lines[signature].append(segment)

    def _compute_points_and_counters(self, segments):
        """
        prepares for sweeping through line of aligned segments
        """
        # loop through each segment
        # we record all points
        # and for each one how many segments start here and end here
        for segment in segments:
            endpoints = segment.get_endpoints()
            self.points[endpoints[0]] = endpoints[0]
            self.points[endpoints[1]] = endpoints[1]
            for counter, point in zip(self.counters, endpoints):
                counter[point] += 1

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

        for point in self.sorted_points:
            now_on = currently_on
            if point in self.counters[START]:
                now_on = now_on + self.counters[START][point]
            if point in self.counters[END]:
                now_on = now_on - self.counters[END][point]
            if currently_on % 2 == 1:
                if currently_on > 0:
                    odd_segments.append(Segment([previous_point, point]))
                else:
                    odd_segments.append(Segment([point, previous_point]))
            previous_point = point
            currently_on = now_on

        if __debug__:
            if is_module_debugged(__name__):
                tycat(self.segments, odd_segments)
        return odd_segments

    def merge(self):
        """
        apply merging algorithm.
        """
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
    merger = SegmentMerger(segments)
    return merger.merge()

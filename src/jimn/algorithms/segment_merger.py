"""
handling overlapping segments.
"""
from collections import defaultdict
from jimn.segment import Segment
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged


class SegmentMerger:
    """
    holds one execution of algorithm merging overlapping segments.
    """
    def __init__(self, segments):
        self.segments = segments
        self.lines = defaultdict(list)
        self.counters = None
        self.sorted_points = None

    def _hash_segments(self):
        """
        hashes aligned segments together
        """
        for segment in self.segments:
            signature = segment.line_hash()
            self.lines[signature].append(segment)

    def _compute_points_and_counters(self, segments):
        """
        prepares for sweeping through line of aligned segments
        """
        # loop through each segment
        # we record all points
        # and for each one how many segments start here and end here
        for segment in segments:
            for point, value in zip(segment.endpoints, (1, -1)):
                self.counters[point] += value

        # now sort points
        self.sorted_points = sorted(self.counters.keys())

    def _odd_segments_on_line(self, line_hash):
        """
        sweeps through line of aligned segments.
        keeping the ones we want
        """
        # associate #starting - #ending segments to each point
        self.counters = defaultdict(int)
        segments = self.lines[line_hash]
        self._compute_points_and_counters(segments)
        # now iterate through each point
        # we record on how many segments we currently are
        previously_on = 0
        # we record where interesting segment started
        previous_point = None

        odd_segments = []

        for point in self.sorted_points:
            now_on = previously_on
            now_on += self.counters[point]
            if previously_on % 2 == 1:
                if now_on % 2 == 0:
                    odd_segments.append(Segment([previous_point, point]))
            else:
                previous_point = point

            previously_on = now_on

        if __debug__:
            if is_module_debugged(__name__):
                tycat(self.segments, odd_segments)
        return odd_segments

    def merge(self):
        """
        apply algorithm.
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
        - odd number of segments overlap -> keep.
    assumes segments' endpoints are sorted
    """
    merger = SegmentMerger(segments)
    return merger.merge()

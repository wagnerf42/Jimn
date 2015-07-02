from jimn.coordinates_hash import coordinates_hash
from jimn.bounding_box import bounding_box
from jimn.debug import is_module_debugged
from jimn.displayable import tycat
from collections import defaultdict
import itertools


class segments_set:
    def __init__(self, segments):
        self.segments = segments

    def get_segments(self):
        return self.segments

    def get_bounding_box(self):
        if not self.segments:
            return
        box = bounding_box.empty_box(self.segments[0].dimension())
        for s in self.segments:
            for p in s.get_endpoints():
                box.add_point(p)
        return box

    def save_svg_content(self, display, color):
        for s in self.segments:
            s.save_svg_content(display, color)

    """brute force algorithm splitting all into elementary segments"""
    def compute_elementary_segments(self):
        new_points = defaultdict(list)
        self.find_new_points(new_points)
        return self.split_segments_at(new_points)

    def split_segments_at(self, new_points):
        elementary_segments = []
        for s in self.segments:
            if s not in new_points:
                elementary_segments.append(s)
            else:
                elementary_segments.extend(s.split_at(new_points[s]))
        return segments_set(elementary_segments)

    def find_new_points(self, new_points):
        rounder = coordinates_hash(dimension=2)
        for pair in itertools.combinations(self.segments, 2):
            i = pair[0].intersection_with(pair[1], rounder)
            if i is not None:
                if __debug__:
                    if is_module_debugged(__name__):
                        print("splitting here:")
                        tycat(self, *pair)
                for s in pair:
                    if not s.has_extremity(i):
                        new_points[s].append(i)

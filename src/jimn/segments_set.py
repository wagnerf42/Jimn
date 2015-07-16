from jimn.coordinates_hash import coordinates_hash
from jimn.bounding_box import bounding_box
from jimn.debug import is_module_debugged
from jimn.displayable import tycat
from collections import defaultdict
from jimn.iterators import two_arrays_combinations


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
    def compute_elementary_segments(self, intersecting_segments):
        intersection_points = self.find_new_points(intersecting_segments)
        return self.split_segments_at(intersection_points)

    def split_segments_at(self, new_points):
        elementary_segments = []
        for s in self.segments:
            if s not in new_points:
                elementary_segments.append(s)
            else:
                elementary_segments.extend(s.split_at(new_points[s]))
        return segments_set(elementary_segments)

    def find_new_points(self, intersecting_segments):
        new_points = defaultdict(list)
        rounder = coordinates_hash(dimension=2)
        for s1, s2 in two_arrays_combinations(self.segments, intersecting_segments):
            i = s1.intersection_with(s2, rounder)
            if i is not None:
                new_points[s1].append(i)
                if __debug__:
                    if is_module_debugged(__name__):
                        print("splitting here:")
                        tycat(self, s1, i)
        return new_points

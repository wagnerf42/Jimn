from jimn.coordinates_hash import coordinates_hash
from jimn.bounding_box import bounding_box
from jimn.debug import is_module_debugged
from jimn.displayable import tycat


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
        rounder = coordinates_hash(dimension=2)
        unclassified_segments = dict(zip(self.segments, self.segments))
        elementary_segments = []
        while unclassified_segments:
            key, candidate_elementary_segment = unclassified_segments.popitem()
            smaller_segments = self.try_splitting(candidate_elementary_segment,
                                                  unclassified_segments,
                                                  rounder)
            if smaller_segments:
                for s in smaller_segments:
                    unclassified_segments[s] = s
            else:
                elementary_segments.append(candidate_elementary_segment)

        return segments_set(elementary_segments)

    # private, used to compute elementary segments
    def try_splitting(self, candidate_elementary_segment, tested_segments, rounder):

        for test_segment in tested_segments.values():
            i = candidate_elementary_segment.intersection_with(
                test_segment,
                rounder
            )
            if i is not None:
                smaller_segments = []
                smaller_segments.extend(candidate_elementary_segment.split_at(i))
                smaller_segments.extend(test_segment.split_at(i))

                if len(smaller_segments) != 2:
                    # they do not intersect, only at endpoints
                    if __debug__:
                        if is_module_debugged(__name__):
                            print("splitting here:")
                            tycat(self, candidate_elementary_segment, test_segment)
                    del tested_segments[test_segment]
                    return smaller_segments
        return []

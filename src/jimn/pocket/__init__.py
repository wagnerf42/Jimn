from jimn.bounding_box import bounding_box
from jimn.polygon import polygon
from jimn.segment import segment
from jimn.algorithms.segment_merger import merge_segments
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import is_almost

"""
set of paths defining a pocket to mill.
"""


class pocket:
    def __init__(self, paths):
        self.paths = paths

    def get_points(self):
        for p in self.paths:
            yield p.get_endpoint(0)

    def round_points(self, rounder):
        for p in self.paths:
            p.round_points(rounder)

    def to_polygon(self):
        return polygon(list(self.get_points()))

    def is_oriented_clockwise(self):
        return self.to_polygon().is_oriented_clockwise()

    def get_bounding_box(self):
        box = bounding_box.empty_box(2)
        for p in self.paths:
            box.update(p.get_bounding_box())
        return box

    def save_svg_content(self, display, color):
        for p in self.paths:
            p.save_svg_content(display, color)

    def remove_overlapping_segments(self):
        """remove overlapping parts of segments
        leaves arcs untouched
        """
        arcs = []
        segments = []
        for p in self.paths:
            if isinstance(p, segment):
                segments.append(p)
            else:
                arcs.append(p)
        self.paths = merge_segments(segments)
        self.paths.extend(arcs)

    def extend(self, additional_paths):
        """
        adds some more paths
        """
        self.paths.extend(additional_paths)

    def get_content(self):
        """
        returns paths contained
        """
        return self.paths

    def is_included_in(self, other):
        tested_point = self.paths[0].get_endpoint(0)
        x, y = tested_point.get_coordinates()

        above_paths = 0
        for p in other.paths:
            x1, x2 = sorted([end.get_x() for end in p.get_endpoints()])
            if x1 == x2:
                continue
            if x > x1 and ((x < x2) or is_almost(x, x2)):
                # we only take paths over us
                intersection_y = p.vertical_intersection_at(x)
                assert not is_almost(intersection_y, y)
                if intersection_y < y:
                    above_paths = above_paths + 1
        included = ((above_paths % 2) == 1)
        if __debug__:
            if is_module_debugged(__name__):
                print("inclusion:", included)
                tycat(self, other)
        return included

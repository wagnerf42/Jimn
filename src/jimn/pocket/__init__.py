"""
set of paths defining a pocket to mill.
"""


class pocket:
    def __init__(self, paths):
        self.paths = paths

    def get_first_point(self):
        """
        returns first point of first path.
        """
        return self.paths[0].get_endpoint(0)

    def get_points(self):
        for p in self.paths:
            yield p.get_endpoint(0)

    def get_dot_label(self):
        """
        returns text label for display in dot file (see polygontree class)
        """
        return ("\"{}\"".format(id(self)))

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
            pbox = p.get_bounding_box()
            box.update(pbox)
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

    def intersects(self, other):
        """
        computes list of intersections between paths in self
        and paths in other.
        does not care about overlappings
        """
        intersections = []
        for p1 in self.paths:
            for p2 in other.paths:
                i = p1.intersections_with(p2, rounder2d)
                intersections.extend(i)
        return intersections

    def is_included_in(self, other):
        """
        fast inclusion test.
        many pre-conditions.
        returns true if we can find one point of self included in other.
        """
        # loop trying points
        for p in self.paths:
            tested_point = p.get_endpoint(0)
            test_result = other._contains_point(tested_point)
            if test_result is not None:
                included = test_result
                break
        else:
            # TODO: big bug here
            # there is not enough info to return true
            # all points of self are on edge of other
            included = True

        if __debug__:
            if is_module_debugged(__name__):
                print("inclusion:", included)
                tycat(self, other)

        return included

    def _contains_point(self, tested_point):
        """
        returns true if point is strictly in self.
        false if strictly out of self.
        none if on self
        """
        x, y = tested_point.get_coordinates()
        above_paths = 0  # simple ray casting algorithm
        for p in self.paths:
            if p.contains(tested_point):
                return None
            x1, x2 = sorted([end.get_x() for end in p.get_endpoints()])
            # skip vertical paths
            if is_almost(x1, x2):
                continue
            if (not is_almost(x, x1)) and x > x1 and ((x < x2) or
                                                      is_almost(x, x2)):
                # we only take paths over us
                # first point not taken into account
                intersection_y = p.vertical_intersection_at(x)
                if intersection_y < y:
                    above_paths = above_paths + 1
        return ((above_paths % 2) == 1)

from jimn.utils.coordinates_hash import rounder2d
from jimn.bounding_box import bounding_box
from jimn.polygon import polygon
from jimn.segment import segment
from jimn.algorithms.segment_merger import merge_segments
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import is_almost

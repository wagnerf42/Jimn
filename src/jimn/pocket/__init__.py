"""
set of paths defining a pocket to mill.
"""


class pocket:
    def __init__(self, paths):
        self.paths = paths

    def split_at_milling_points(self, milling_diameter, paths):
        """
        splits each path of the pocket at milling heights.
        adds results to given paths array
        """
        for p in self.paths:
            paths.extend(p.split_at_milling_points(milling_diameter))

    def translate(self, translation):
        """
        translates the whole pocket by a given translation vector.
        returns new pocket if obtained pocket is different and same pocket
        if translation vector is (0,0)
        """
        if translation.is_almost(point([0, 0])):
            return self
        return pocket([p.translate(translation) for p in self.paths])

    def get_points(self):
        for p in self.paths:
            yield p.get_endpoint(0)

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

    def of_reversed_arcs(self):
        """
        returns true if we are made only of reversed arcs.
        returns false if we contain no reversed arcs
        raises an exception if we contain some reversed arcs
        """
        seen_reversed_arcs = False
        seen_non_reversed = False
        for p in self.paths:
            if isinstance(p, arc):
                if p.reversed_direction:
                    seen_reversed_arcs = True
                else:
                    seen_non_reversed = True
            else:
                seen_non_reversed = True

        if seen_reversed_arcs and seen_non_reversed:
            raise Exception("both reversed and non reversed")
        return seen_reversed_arcs

    def remove_overlap_with(self, other):
        """
        cancel all overlapping parts in segments
        """
        if not self.get_bounding_box().intersects(other.get_bounding_box()):
            return
        # filter arcs
        arcs = [[], []]
        segments = [[], []]
        for i, paths in enumerate([self.paths, other.paths]):
            for p in paths:
                if isinstance(p, segment):
                    segments[i].append(p)
                else:
                    arcs[i].append(p)

        # now remove overlap in segments
        kept_segments = []  # kept segments in self
        while(segments[0]):
            s1 = segments[0].pop()
            for s2 in segments[1]:
                remains = s1.remove_overlap_with(s2)  # TODO: good enough ?
                if remains:
                    segments[1].remove(s2)
                    for s, r in zip(segments, remains):
                        s.extend(r)
                    break
            else:
                kept_segments.append(s1)
        arcs[0].extend(kept_segments)
        arcs[1].extend(segments[1])
        self.paths = arcs[0]

        if id(self) != id(other):
            other.paths = arcs[1]

    def intersections_with(self, other, results):
        """
        intersect self and other.
        results is a hash table associating to each path a list of
        intersection points.
        also works when self is other
        """
        if not self.get_bounding_box().intersects(other.get_bounding_box()):
            return
        for p1 in self.paths:
            for p2 in other.paths:
                intersections = p1.intersections_with(p2)
                if intersections:
                    rounded_intersections = [
                        rounder2d.hash_point(i) for i in intersections
                    ]
                    results[id(p1)].extend(rounded_intersections)
                    if id(p1) != id(p2):
                        results[id(p2)].extend(rounded_intersections)

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

    def __hash__(self):
        h = hash(tuple(self.paths))
        return h

    def __eq__(self, other):
        if len(self.paths) != len(other.paths):
            return False
        sorted_self = sorted(self.paths)
        sorted_other = sorted(other.paths)
        for i in range(len(sorted_self)):
            if sorted_self[i] != sorted_other[i]:
                return False
        return True

    def __str__(self):
        return "pocket([\n    " + ",\n    ".join([str(p) for p in self.paths]) \
            + "\n])"

from jimn.bounding_box import bounding_box
from jimn.polygon import polygon
from jimn.point import point
from jimn.arc import arc
from jimn.segment import segment
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import is_almost
from jimn.utils.coordinates_hash import rounder2d

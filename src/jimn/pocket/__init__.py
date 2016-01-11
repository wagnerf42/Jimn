"""
set of paths defining a pocket to mill.
"""

from itertools import combinations
from jimn.bounding_box import Bounding_Box
from jimn.polygon import polygon
from jimn.point import Point
from jimn.arc import arc
from jimn.segment import Segment
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import is_almost
from jimn.utils.iterators import all_combinations


class pocket:
    def __init__(self, paths):
        self.paths = paths

    def split_at_milling_points(self, milling_diameter, paths):
        """
        splits each path of the pocket at milling heights.
        adds results to given paths array
        """
        for path in self.paths:
            paths.extend(path.split_at_milling_points(milling_diameter))

    def translate(self, translation):
        """
        translates the whole pocket by a given translation vector.
        returns new pocket if obtained pocket is different and same pocket
        if translation vector is (0,0)
        """
        if translation.is_almost(Point([0, 0])):
            return self
        return pocket([p.translate(translation) for p in self.paths])

    def get_points(self):
        """
        iterates through each point in the pocket
        """
        for path in self.paths:
            yield path.get_endpoint(0)

    def to_polygon(self):
        """
        turn into a polygon by joining all points.
        only meaningful if paths inside are sorted
        in the right order
        """
        return polygon(list(self.get_points()))

    def is_oriented_clockwise(self):
        """
        true if we are oriented clockwise.
        only meaningful if paths inside are sorted
        in the right order
        """
        return self.to_polygon().is_oriented_clockwise()

    def get_bounding_box(self):
        """
        returns min bounding box containing pocket
        """
        box = Bounding_Box.empty_box(2)
        for path in self.paths:
            pbox = path.get_bounding_box()
            box.update(pbox)
        return box

    def save_svg_content(self, display, color):
        """
        svg code for tycat
        """
        for path in self.paths:
            path.save_svg_content(display, color)

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
        for path in self.paths:
            tested_point = path.get_endpoint(0)
            test_result = other.contains_point(tested_point)
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
        for path in self.paths:
            if isinstance(path, arc):
                if path.reversed_direction:
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
            for path in paths:
                if isinstance(path, Segment):
                    segments[i].append(path)
                else:
                    arcs[i].append(path)

        # now remove overlap in segments
        kept_segments = []  # kept segments in self
        while segments[0]:
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

    def self_intersections(self, results):
        return _iterated_intersections(results, combinations(self.paths, 2))

    def intersections_with(self, other, results):
        """
        intersect self and other.
        results is a hash table associating to each path a list of
        intersection points.
        """
        if not self.get_bounding_box().intersects(other.get_bounding_box()):
            return
        return _iterated_intersections(results, all_combinations(self.paths,
                                                                 other.paths))

    def split_at(self, intersections):
        new_paths = []
        for p in self.paths:
            if id(p) in intersections:
                new_paths.extend(p.split_at(intersections[id(p)]))
            else:
                new_paths.append(p)
        self.paths = new_paths

    def contains_point(self, tested_point):
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


def _iterated_intersections(results, iterator):
    for p1, p2 in iterator:
        intersections = p1.intersections_with(p2)
        if intersections:
            for i in intersections:
                if not(p1.endpoints[0].is_almost(i)
                       or p1.endpoints[1].is_almost(i)):
                    results[id(p1)].append(i)

            if id(p1) != id(p2):
                for i in intersections:
                    if not(p2.endpoints[0].is_almost(i)
                           or p2.endpoints[1].is_almost(i)):
                        results[id(p2)].append(i)

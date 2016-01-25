"""
set of paths defining a pocket to mill.
"""
from itertools import combinations
from jimn.bounding_box import Bounding_Box
from jimn.polygon import Polygon
from jimn.point import Point
from jimn.arc import Arc
from jimn.segment import Segment
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import is_almost
from jimn.utils.iterators import all_combinations
from jimn.utils.coordinates_hash import rounder2d


class Pocket:
    """
    ordered set of paths defining the edge of an area to mill.
    """
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
        return Pocket([p.translate(translation) for p in self.paths])

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
        return Polygon(list(self.get_points()))

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
        return true if we are made only of reversed arcs.
        return false if we contain no reversed arcs
        raise an exception if we contain some reversed arcs
        """
        seen_reversed_arcs = False
        seen_non_reversed = False
        for path in self.paths:
            if isinstance(path, Arc):
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
        cancel all overlapping parts in segments.
        destroys paths ordering.
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
            segment1 = segments[0].pop()
            for segment2 in segments[1]:
                remains = segment1.remove_overlap_with(segment2)
                if remains:
                    segments[1].remove(segment2)
                    for segment_set, remain in zip(segments, remains):
                        segment_set.extend(remain)
                    break
            else:
                kept_segments.append(segment1)
        arcs[0].extend(kept_segments)
        arcs[1].extend(segments[1])
        self.paths = arcs[0]

        if id(self) != id(other):
            other.paths = arcs[1]

    def self_intersections(self, results):
        """
        compute intersections between paths we contain and store
        them in results.
        """
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
        """
        turn paths into more elementary paths by splitting them at
        given intersections.
        for speed reasons intersections are already associated to paths.
        """
        new_paths = []
        for path in self.paths:
            if id(path) in intersections:
                new_paths.extend(path.split_at(intersections[id(path)]))
            else:
                new_paths.append(path)
        self.paths = new_paths

    def contains_point(self, tested_point):
        """
        return true if point is strictly in self.
        false if strictly out of self.
        none if on self.
        """
        point_x, point_y = tested_point.get_coordinates()
        above_paths = 0  # simple ray casting algorithm
        for path in self.paths:
            if path.contains(tested_point):
                return None
            x_1, x_2 = sorted([end.get_x() for end in path.get_endpoints()])
            # skip vertical paths
            if is_almost(x_1, x_2):
                continue
            if (not is_almost(point_x, x_1)) and point_x > x_1\
                    and ((point_x < x_2) or is_almost(point_x, x_2)):
                # we only take paths over us
                # first point not taken into account
                intersection_y = path.vertical_intersection_at(point_x)
                if intersection_y < point_y:
                    above_paths = above_paths + 1
        return (above_paths % 2) == 1

    def __hash__(self):
        return hash(tuple(self.paths))

    def __eq__(self, other):
        if len(self.paths) != len(other.paths):
            return False
        sorted_self = sorted(self.paths)
        sorted_other = sorted(other.paths)
        for index in range(len(sorted_self)):
            if sorted_self[index] != sorted_other[index]:
                return False
        return True

    def __str__(self):
        return "Pocket([\n    " + ",\n    ".join([str(p) for p in self.paths]) \
            + "\n])"


def _iterated_intersections(results, iterator):
    for path1, path2 in iterator:
        intersections = path1.intersections_with(path2)
        if intersections:
            for i in intersections:
                if not(path1.endpoints[0].is_almost(i)
                       or path1.endpoints[1].is_almost(i)):
                    results[id(path1)].append(rounder2d.hash_point(i))

                if not(path2.endpoints[0].is_almost(i)
                       or path2.endpoints[1].is_almost(i)):
                    results[id(path2)].append(rounder2d.hash_point(i))

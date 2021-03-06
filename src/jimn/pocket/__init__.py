"""
set of paths defining a pocket to mill.
"""
from random import random
from itertools import combinations
from jimn.bounding_box import BoundingBox
from jimn.polygon import Polygon
from jimn.point import Point
from jimn.segment import Segment
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import is_almost
from jimn.utils.iterators import all_combinations
from jimn.utils.coordinates_hash import ROUNDER2D, CoordinatesHash
from jimn.utils.caching import cached, invalidate_cache


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
            yield path.endpoints[0]

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

    @cached
    def get_bounding_box(self):
        """
        returns min bounding box containing pocket
        """
        box = BoundingBox.empty_box(2)
        for path in self.paths:
            pbox = path.get_bounding_box()
            box.update(pbox)
        return box

    def svg_content(self):
        """
        svg code for tycat
        """
        return "".join(p.svg_content() for p in self.paths)

    @invalidate_cache
    def extend(self, additional_paths):
        """
        adds some more paths
        """
        self.paths.extend(additional_paths)

    def vertical_intersections(self, intersecting_x):
        """
        return all y coordinates for paths intersecting vertical
        line at given x.
        precondition: given x is not the one of any point in self.
        """
        intersecting_ys = list()
        for path in self.paths:
            xmin, xmax = sorted([end.get_x() for end in path.endpoints])
            if intersecting_x > xmin and intersecting_x < xmax:
                intersecting_y = path.vertical_intersection_at(intersecting_x)
                intersecting_ys.append(intersecting_y)
        return intersecting_ys

    def inside_point_not_on(self, x_hash):
        """
        return a random point STRICTLY inside pocket whose
        x coordinate is not in given hash.
        """
        # take a x coordinate not in given hash
        x_coordinates = [p.coordinates[0]
                         for path in self.paths
                         for p in path.endpoints]
        xmin = min(x_coordinates)
        xmax = max(x_coordinates)
        chosen_x = None
        while chosen_x is None or x_hash.contains_coordinate(chosen_x):
            factor = random()
            chosen_x = xmin*factor + (1-factor)*xmax

        # now, intersect vertically
        # to figure which y ranges are on the inside
        intersections = sorted(self.vertical_intersections(chosen_x))
        # take point in first range
        chosen_y = (intersections[0] + intersections[1]) / 2
        return Point([chosen_x, chosen_y])

    def is_included_in(self, other):
        """
        inclusion test.
        precondition: self and other do not intersect and don't share edges.
        """
        # pre-test using bounding box
        box = other.get_bounding_box()
        if not box.contains(self.get_bounding_box()):
            return False

        x_hash = CoordinatesHash()
        for pocket in (self, other):
            for point in pocket.get_points():
                x_hash.hash_coordinate(point.get_x())

        inside_point = self.inside_point_not_on(x_hash)
        included = other.contains_point(inside_point)

        if __debug__:
            if is_module_debugged(__name__):
                print("inclusion:", included)
                tycat(self, other)

        return included

    def remove_overlap_with(self, other):
        """
        cancel all overlapping parts in segments in both self and other.
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
                if segment1.overlaps(segment2):
                    remains1 = segment1.remove_overlap_with(segment2)
                    remains2 = segment2.remove_overlap_with(segment1)
                    segments[1].remove(segment2)
                    segments[1].extend(remains2)
                    segments[0].extend(remains1)
                    break
            else:
                kept_segments.append(segment1)
        arcs[0].extend(kept_segments)
        arcs[1].extend(segments[1])
        self.paths = arcs[0]

        if id(self) != id(other):
            other.paths = arcs[1]

    def remove_self_overlap(self):
        """
        remove all overlapping parts in our own segments.
        """
        # filter arcs
        arcs = []
        segments = []
        for path in self.paths:
            if isinstance(path, Segment):
                segments.append(path)
            else:
                arcs.append(path)

        kept_segments = []  # kept segments in self
        while segments:
            segment1 = segments.pop()
            for segment2 in segments:
                if segment1.overlaps(segment2):
                    remains = segment1.remove_overlap_with(segment2)
                    segments.extend(remains)
                    break
            else:
                kept_segments.append(segment1)
        arcs.extend(kept_segments)
        self.paths = arcs

    def self_intersections(self, results):
        """
        compute intersections between paths we contain and store
        them in results.
        """
        _iterated_intersections(results, combinations(self.paths, 2))

    def intersections_with(self, other, results):
        """
        intersect self and other.
        results is a hash table associating to each path a list of
        intersection points.
        """
        if not self.get_bounding_box().intersects(other.get_bounding_box()):
            return
        _iterated_intersections(results, all_combinations(self.paths,
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
        precondition: point is not on self.
        """
        point_x, point_y = tested_point.coordinates
        above_paths = 0  # simple ray casting algorithm
        for path in self.paths:
            x_1, x_2 = sorted([end.get_x() for end in path.endpoints])
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

        sorted_self = sorted(self.paths, key=lambda path: path.endpoints)
        sorted_other = sorted(other.paths, key=lambda path: path.endpoints)
        for our_path, other_path in zip(sorted_self, sorted_other):
            if our_path != other_path:
                return False
        return True

    def __str__(self):
        strings = ",\n    ".join(str(p) for p in self.paths)
        return "Pocket([\n    " + strings + "\n])"


def _iterated_intersections(results, iterator):
    for path1, path2 in iterator:
        intersections = path1.intersections_with(path2)
        if intersections:
            for i in intersections:
                if not(path1.endpoints[0].is_almost(i) or
                       path1.endpoints[1].is_almost(i)):
                    results[id(path1)].append(ROUNDER2D.hash_point(i))

                if not(path2.endpoints[0].is_almost(i) or
                       path2.endpoints[1].is_almost(i)):
                    results[id(path2)].append(ROUNDER2D.hash_point(i))

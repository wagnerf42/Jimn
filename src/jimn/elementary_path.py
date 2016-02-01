"""
base class for segments or arcs.
basic path between two endpoints (oriented).
"""
from copy import copy
from jimn.displayable import tycat
from jimn.utils.iterators import all_two_elements
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import is_almost, SEGMENT_LIMIT
from jimn.point import Point


class ElementaryPath:
    """
    elementary path is a small path between two endpoints.
    class is further refined into segments and arcs.
    """
    def __init__(self, points):
        self.endpoints = points
        assert self.length() > SEGMENT_LIMIT, "very small path"

    def length(self):
        """
        return length of path.
        """
        return self.endpoints[0].distance_to(self.endpoints[1])

    def get_endpoint_not(self, point):
        """
        return endpoint not given point.
        requires path to contain given point.
        """
        if self.endpoints[0] == point:
            return self.endpoints[1]
        if self.endpoints[1] == point:
            return self.endpoints[0]
        raise Exception("no given endpoint ; cannot find other")

    def squared_distance_from_start(self, point):
        """
        returns scalar used for comparing points on path.
        the higher the scalar, the closer to endpoint.
        """
        return self.endpoints[0].squared_distance_to(point)

    def split_at(self, points):
        """
        split path at given points.
        returns list of same type objects ;
        orientation is kept ;
        assumes points are on path ;
        outside points are not added ;
        input points can be duplicated but no output paths are
        """
        start_point, end_point = self.endpoints

        all_points = list(points)
        all_points.extend(self.endpoints)
        sorted_points = sorted(
            all_points, key=self.squared_distance_from_start
        )
        paths = []  # result
        inside = False
        for p_1, p_2 in all_two_elements(sorted_points):
            if p_1 == start_point:
                inside = True
            if p_1 == end_point:
                inside = False
            if inside and not p_1.is_almost(p_2):
                new_path = copy(self)
                new_path.endpoints = [p_1, p_2]
                if __debug__:
                    if new_path.length() < SEGMENT_LIMIT:
                        print("splitting", self, "at", [str(p) for p in points])
                        tycat(self, *points)
                        raise Exception("very small path when splitting")
                paths.append(new_path)

        if __debug__:
            if is_module_debugged(__name__):
                print("splitting path:")
                tycat(self, *paths)
        return paths

    def split_around(self, intermediate_point):
        """
        returns two subpaths: getting to intermediate and from intermediate
        to end. if intermediate is one endpoint, one of the returned path
        will be 'None'
        """
        after = None
        before = None
        if intermediate_point.is_almost(self.endpoints[0]):
            after = self
        elif intermediate_point.is_almost(self.endpoints[1]):
            before = self
        else:
            split_path = self.split_at([intermediate_point])
            before, after = split_path[0:2]
        return (before, after)

    def _find_common_xrange(self, other):
        """
        find three different x belonging to both paths.
        """
        self_x = sorted([p.get_x() for p in self.endpoints])
        other_x = sorted([p.get_x() for p in other.endpoints])
        max_x_of_starts = max(self_x[0], other_x[0])
        min_x_of_ends = min(self_x[1], other_x[1])
        return [
            max_x_of_starts,
            (max_x_of_starts + min_x_of_ends) / 2,
            min_x_of_ends
        ]

    def is_above(self, other):
        """
        returns true if self is strictly above other.
        will not work on overlapping vertical segments.
        """
        # take three common x
        # we need three because arcs are not flat
        # and need therefore three comparison points
        common_abs = self._find_common_xrange(other)
        # compute y intersections
        self_y, other_y = [
            [s.vertical_intersection_at(x) for x in common_abs]
            for s in (self, other)
        ]
        # who is above whom
        point_above = [0, 0, 0]
        for i in range(3):
            if is_almost(self_y[i], other_y[i]):
                point_above[i] = 0
            else:
                if self_y[i] < other_y[i]:
                    point_above[i] = 1
                else:
                    point_above[i] = -1
        # assert all points are on one same side
        possibly_below = True
        possibly_above = True
        for being_above in point_above:
            if being_above > 0:
                possibly_below = False
            if being_above < 0:
                possibly_above = False

        if __debug__:
            if not (possibly_below or possibly_above):
                print("failed above test: ", self, other)
                print(common_abs)
                print(self_y)
                print(other_y)
                tycat(self, other)
                tycat(other,
                      [Point([common_abs[i], other_y[i]]) for i in range(3)])
                raise Exception("cannot see which path is above the other")

        globaly_above = sum(point_above)
        if globaly_above == 0:
            return False
        return globaly_above > 0

    def is_vertical(self):
        """
        are endpoints aligned vertically ?
        """
        x_1, x_2 = [p.get_x() for p in self.endpoints]
        if x_1 == x_2:
            return True
        else:
            if is_almost(x_1, x_2):
                raise RuntimeError("almost vertical")
            return False

    def is_almost_horizontal(self):
        """
        are endpoints almost aligned horizontally ?
        """
        return is_almost(*[p.get_y() for p in self.endpoints])

    def lowest_endpoint(self):
        """
        return one of lowest endpoints (y maximized).
        """
        y_1, y_2 = [p.get_y() for p in self.endpoints]
        if y_1 > y_2:
            return self.endpoints[0]
        else:
            return self.endpoints[1]

    def adjust_points_at_milling_height(self, milling_height):
        """
        slightly move endpoints so that if they are very close
        from a milling height then they will be exactly at milling height.
        careful : we do not change arc's center point so this might lead
        to rounding errors.
        """
        self.endpoints = [p.adjust_at_milling_height(milling_height)
                          for p in self.endpoints]

    def update_height(self, height):
        # pylint: disable=no-self-use
        """
        height change by following this path (no change since horizontal).
        """
        return height

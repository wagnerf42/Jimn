"""
base class for segments or arcs.
basic path between two endpoints (oriented).
"""
from copy import copy, deepcopy
from jimn.displayable import tycat
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
        requires path to have one endpoint being given point.
        """
        if self.endpoints[0] == point:
            return self.endpoints[1]
        if self.endpoints[1] == point:
            return self.endpoints[0]
        raise Exception("no given endpoint ; cannot find other")

    def distance_from_start(self, point):
        """
        return a scalar used for comparing points on path.
        the higher the scalar, the closer to endpoint.
        """
        return self.endpoints[0].distance_to(point)

    def split_at(self, points):
        """
        split path at given points.
        returns list of same type objects ;
        orientation is kept ;
        assumes points are on path ;
        input points can be duplicated but no output paths are.
        """
        points = list(set([p for a in (self.endpoints, points) for p in a]))
        sorted_points = sorted(points, key=self.distance_from_start)

        paths = []
        for point1, point2 in zip(sorted_points[:-1], sorted_points[1:]):
            path_chunk = deepcopy(self)
            path_chunk.endpoints[0] = copy(point1)
            path_chunk.endpoints[1] = copy(point2)
            paths.append(path_chunk)

        if __debug__:
            if is_module_debugged(__name__):
                print("splitting path:")
                tycat(self, *paths)
        return paths

    def split_around(self, intermediate_point):
        """
        return two subpaths: getting to intermediate and from intermediate
        to end. if intermediate is one endpoint, one of the returned path
        will be 'None'.
        pre-condition : intermediate_point is on path.
        """
        after = None
        before = None
        if intermediate_point.is_almost(self.endpoints[0]):
            after = self
        elif intermediate_point.is_almost(self.endpoints[1]):
            before = self
        else:
            before = deepcopy(self)
            before.endpoints[1] = copy(intermediate_point)
            after = deepcopy(self)
            after.endpoints[0] = copy(intermediate_point)
        return (before, after)

    def height_comparison(self, other):
        """
        return 1 if self is strictly above other.
        return 0 if self and other are overlapping.
        return -1 if self is strictly below other.
        will not work on overlapping vertical segments.
        """

        def find_common_xrange(self, other):
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

        # take three common x
        # we need three because arcs are not flat
        # and need therefore three comparison points
        common_abs = find_common_xrange(self, other)
        # compute y intersections
        self_y, other_y = [
            [s.vertical_intersection_at(x) for x in common_abs]
            for s in (self, other)
        ]
        # who is above whom
        points_above = [0, 0, 0]
        for i in range(3):
            if is_almost(self_y[i], other_y[i]):
                points_above[i] = 0
            else:
                if self_y[i] < other_y[i]:
                    points_above[i] = 1
                else:
                    points_above[i] = -1

        overlapping = len([a for a in points_above if a == 0]) == 3
        if overlapping:
            return 0
        above = len([a for a in points_above if a >= 0]) == 3
        if above:
            return 1
        below = len([a for a in points_above if a <= 0]) == 3
        if below:
            return -1

        print("failed above test: ", self, other)
        print(common_abs)
        print(self_y)
        print(other_y)
        tycat(self, other)
        tycat(other, [Point([common_abs[i], other_y[i]]) for i in range(3)])
        raise Exception("cannot see which path is above the other")

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



from jimn.displayable import tycat
from jimn.utils.iterators import all_two_elements
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import is_almost, segment_limit
from math import sqrt
import copy


class elementary_path:

    """
    elementary path is a small path between two endpoints.
    class is further refined into segments and arcs
    """

    def __init__(self, points):
        self.endpoints = points
        assert self.squared_length() > segment_limit, "very small path"

    def length(self):
        return sqrt(self.squared_length())

    def squared_length(self):
        """squared distance between endpoints"""
        return self.endpoints[0].squared_distance_to(self.endpoints[1])

    def set_endpoint(self, index, new_point):
        """set new_point as endpoint numbered index"""
        self.endpoints[index] = new_point
        assert self.squared_length() > segment_limit, "very small path"

    def get_endpoint(self, index):
        return self.endpoints[index]

    def get_endpoint_not(self, p):
        """
        returns endpoint not p.
        requires other endpoint to be p
        """
        if self.endpoints[0] == p:
            return self.endpoints[1]
        if self.endpoints[1] == p:
            return self.endpoints[0]
        raise Exception("no given endpoint ; cannot find other")

    def get_endpoints(self):
        return self.endpoints

    def get_stored_endpoints(self):
        """
        returns endpoints in stored order.
        we do not care whether the arc is reversed
        or not.
        useful for operations independant from arc direction
        """
        return self.endpoints

    def sort_endpoints(self):
        """sort endpoints and return a new path (same type).
        this will also work in derived classes
        """
        copied_path = copy.deepcopy(self)
        copied_path.endpoints = sorted(self.endpoints)
        return copied_path

    def is_sorted(self):
        sorted_self = self.sort_endpoints()
        return sorted_self.endpoints == self.endpoints

    def dimension(self):
        """returns dimension of space containing the points"""
        return self.endpoints[0].dimension()

    def squared_distance_from_start(self, p):
        """
        returns scalar used for comparing points on path.
        the higher the scalar, the closer to endpoint.
        """
        return self.endpoints[0].squared_distance_to(p)

    def split_at(self, points):
        """split path at given points.
        returns list of same type objects ;
        orientation is kept ;
        assumes points are on path ;
        outside points are not added ;
        input points can be duplicated but no output paths are
        """
        start_point, end_point = self.endpoints

        points.extend(self.endpoints)
        sorted_points = sorted(
            points, key=lambda p: self.squared_distance_from_start(p)
        )
        paths = []
        inside = False
        for p1, p2 in all_two_elements(sorted_points):
            if p1 == start_point:
                inside = True
            if p1 == end_point:
                inside = False
            if inside and not p1.is_almost(p2):
                new_path = copy.copy(self)
                new_path.endpoints = [p1, p2]
                if __debug__:
                    if new_path.squared_length() < segment_limit:
                        print("splitting", self, "at", *points)
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
        if intermediate_point.is_almost(self.get_endpoint(0)):
            after = self
        elif intermediate_point.is_almost(self.get_endpoint(1)):
            before = self
        else:
            before, after = self.split_at([intermediate_point])
        return (before, after)

    def intersections_with(self, other):
        """compute intersections with some other path.
        works with any combination of arcs and segments
        """
        if str(type(self)) == "<class 'jimn.arc.arc'>":
            if str(type(other)) == "<class 'jimn.arc.arc'>":
                intersections = self.intersections_with_arc(other)
            else:
                intersections = self.intersections_with_segment(other)
        else:
            if str(type(other)) == "<class 'jimn.arc.arc'>":
                intersections = other.intersections_with_segment(self)
            else:
                i = self.intersection_with_segment(other)
                if i is None:
                    intersections = []
                else:
                    intersections = [i]

        if __debug__:
            if is_module_debugged(__name__):
                print("intersections are:")
                tycat(self, other, intersections)

        return intersections

    def get_polygon_id(self):
        """returns id of polygon we belong to.
        0 unless overloaded in subclasses
        """
        return 0

    def _find_common_xrange(a, b):
        """
        find three different x belonging to both paths
        """
        xa = sorted([p.get_x() for p in a.endpoints])
        xb = sorted([p.get_x() for p in b.endpoints])
        x1max = max(xa[0], xb[0])
        x2min = min(xa[1], xb[1])
        assert x1max <= x2min
        return [x1max, x1max + (x2min-x1max)/2, x2min]

    def is_above(a, b):
        """
        returns true if a is strictly above b
        """
        # take three common x
        # we need three because arcs are not flat
        # and need therefore three comparison points
        common_abs = a._find_common_xrange(b)
        # compute y intersections
        ya, yb = [
            [s.vertical_intersection_at(x) for x in common_abs]
            for s in (a, b)
        ]
        # who is above whom
        point_above = [0, 0, 0]
        for i in range(3):
            if is_almost(ya[i], yb[i]):
                point_above[i] = 0
            else:
                if ya[i] < yb[i]:
                    point_above[i] = 1
                else:
                    point_above[i] = -1
        # assert all points are on one same side
        possibly_below = True
        possibly_above = True
        for p in point_above:
            if p > 0:
                possibly_below = False
            if p < 0:
                possibly_above = False

        if __debug__:
            if not (possibly_below or possibly_above):
                print("failed above test: ", a, b)
                print(common_abs)
                print(ya)
                print(yb)
                tycat(a, b)
                tycat(b, [point([common_abs[i], yb[i]]) for i in range(3)])
                raise Exception("cannot see which path is above the other")

        s = sum(point_above)
        if s == 0:
            return False
        return (s > 0)

    def is_vertical(self):
        """are endpoints aligned vertically"""
        xa, xb = [p.get_x() for p in self.endpoints]
        if xa == xb:
            return True
        else:
            if is_almost(xa, xb):
                raise RuntimeError("almost vertical")
            return False

    def is_horizontal(self):
        """are endpoints aligned horizontally"""
        ya, yb = [p.get_y() for p in self.endpoints]
        if ya == yb:
            return True
        else:
            if is_almost(ya, yb):
                raise RuntimeError("almost horizontal")
            return False

    def lowest_endpoint(self):
        """
        return one of lowest endpoints (y maximized)
        """
        ya, yb = [p.get_y() for p in self.endpoints]
        if ya > yb:
            return self.endpoints[0]
        else:
            return self.endpoints[1]

    def __hash__(self):
        return hash(tuple(self.endpoints))

    def __eq__(a, b):
        types = [str(type(p)) == "<class 'jimn.arc.arc'>" for p in (a, b)]
        if types[0] and not types[1]:
            return False
        if types[1] and not types[0]:
            return False
        if types[0] and types[1]:
            return a.equals(b)

        return a.endpoints == b.endpoints

    def __lt__(a, b):
        types = [str(type(p)) == "<class 'jimn.arc.arc'>" for p in (a, b)]
        if types[0] and not types[1]:
            return True
        if types[1] and not types[0]:
            return False

        return a.comparison(b)

    def update_height(self, height):
        """
        height change by following this path (no change since horizontal)
        """
        return height

from jimn.point import point

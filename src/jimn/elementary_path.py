from jimn.iterators import all_two_elements
from jimn.precision import segment_limit
from jimn.debug import is_module_debugged
from jimn.displayable import tycat
from jimn.precision import is_almost
import copy


class elementary_path:
    def __init__(self, points):
        self.endpoints = points
        assert self.squared_length() > segment_limit, "very small path"

    def squared_length(self):
        """squared distance between endpoints"""
        return self.endpoints[0].squared_distance_to(self.endpoints[1])

    def reverse(self):
        copied_path = copy.copy(self)
        copied_path.endpoints = list(reversed(self.endpoints))
        return copied_path

    def has_extremity(self, intermediate_point):
        for p in self.endpoints:
            if p == intermediate_point:
                return True
            else:
                assert not p.is_almost(intermediate_point), "precision pb"
        return False

    def set_endpoint(self, index, new_point):
        self.endpoints[index] = new_point

    def get_endpoint(self, index):
        return self.endpoints[index]

    def get_endpoints(self):
        return self.endpoints

    def sort_endpoints(self):
        copied_path = copy.copy(self)
        copied_path.endpoints = sorted(self.endpoints)
        return copied_path

    def is_sorted(self):
        sorted_self = self.sort_endpoints()
        return sorted_self.endpoints == self.endpoints

    def dimension(self):
        return self.endpoints[0].dimension()

    def split_at(self, points):
        """split path at given points.
        returns list of same type objects ;
        orientation is kept ;
        assumes points are on path ;
        outside points are not added ;
        input points can be duplicated but no output paths are
        """
        start_point, end_point = self.endpoints
        d = end_point - start_point

        points.extend(self.endpoints)
        sorted_points = sorted(points, key=lambda p: p.scalar_product(d))
        paths = []
        inside = False
        for p1, p2 in all_two_elements(sorted_points):
            if p1 == start_point:
                inside = True
            if p1 == end_point:
                inside = False
            if inside and not p1 == p2:
                new_path = copy.copy(self)
                new_path.endpoints = [p1, p2]
                assert new_path.squared_length() > segment_limit,\
                    "very small path when splitting"
                paths.append(new_path)

        if __debug__:
            if is_module_debugged(__name__):
                print("splitting path:")
                tycat(self, *paths)
        return paths

    def intersections_with(self, other, rounder):
        points = [end for p in (self, other) for end in p.get_endpoints()]
        for p in points:
            rounder.hash_point(p)
        if str(type(self)) == "<class 'jimn.arc.arc'>":
            if str(type(other)) == "<class 'jimn.arc.arc'>":
                intersections = self.intersections_with_arc(other, rounder)
            else:
                intersections = self.intersections_with_segment(other, rounder)
        else:
            if str(type(other)) == "<class 'jimn.arc.arc'>":
                intersections = other.intersections_with_segment(self, rounder)
            else:
                i = self.intersection_with_segment(other, rounder)
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
        return 0

    def is_above(a, b):
        xa = sorted([p.get_x() for p in a.endpoints])
        xb = sorted([p.get_x() for p in b.endpoints])
        x1max = max(xa[0], xb[0])
        x2min = min(xa[1], xb[1])
        common_abs = [x1max, x2min]
        ya, yb = [
            [s.vertical_intersection_at(x) for x in common_abs]
            for s in (a, b)
        ]
        return ya < yb

    def is_vertical(self):
        xa, xb = [p.get_x() for p in self.endpoints]
        if xa == xb:
            return True
        else:
            if is_almost(xa, xb):
                raise RuntimeError("almost vertical")
            return False

    def is_horizontal(self):
        ya, yb = [p.get_y() for p in self.endpoints]
        if ya == yb:
            return True
        else:
            if is_almost(ya, yb):
                raise RuntimeError("almost vertical")
            return False

    def is_above_y(self, y_limit):
        non_limit_y = None
        for p in self.endpoints:
            y = p.get_y()
            if y != y_limit:
                non_limit_y = y
        assert non_limit_y is not None, "horizontal path"
        return non_limit_y < y_limit

    def lowest_endpoint(self):
        ya, yb = [p.get_y() for p in self.endpoints]
        if ya > yb:
            return self.endpoints[0]
        else:
            return self.endpoints[1]

    def is_same(self, other):
        if self.get_endpoints() == other.get_endpoints():
            return True
        if self.get_endpoints() == other.reverse().get_endpoints():
            return True
        return False

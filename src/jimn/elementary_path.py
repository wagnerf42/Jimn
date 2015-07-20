from jimn.iterators import all_two_elements
from jimn.precision import segment_limit
from jimn.debug import is_module_debugged
from jimn.displayable import tycat
from jimn.precision import check_precision, is_almost
from jimn.point import point
import copy


class elementary_path:
    def __init__(self, points):
        self.endpoints = points
        if __debug__:
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
                paths.append(new_path)

        if __debug__:
            if is_module_debugged(__name__):
                print("splitting path:")
                tycat(self, *paths)
        return paths

    def intersections_with(self, other, rounder):
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

    def winding_number(self):
        d = self.endpoints[1] - self.endpoints[0]
        w = d.cross_product(point([0, 1]))
        if is_almost(w, 0):
            return 0
        if w > 0:
            return 1
        else:
            return -1

    def is_above(a, b):
        xa = sorted([p.get_x() for p in a.endpoints])
        xb = sorted([p.get_x() for p in b.endpoints])
        x1max = max(xa[0], xb[0])
        x2min = max(xa[1], xb[1])
        common_abs = [x1max, x2min]
        ya, yb = [
            [s.vertical_intersection_at(x) for x in common_abs]
            for s in (a, b)
        ]
        return ya < yb

    def vertical_intersection_at(self, x):
        # TODO: not ok for arcs
        x1, y1 = self.endpoints[0].get_coordinates()
        x2, y2 = self.endpoints[1].get_coordinates()
        if x1 == x2:
            # when vertical, we return lowest coordinate
            return y1
        if __debug__:
            check_precision(x1, x2, 'vertical_intersection_at')
        a = (y2-y1)/(x2-x1)
        y = y1 + a*(x-x1)
        return y

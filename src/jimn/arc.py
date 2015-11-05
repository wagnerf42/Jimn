from jimn.elementary_path import elementary_path


class arc(elementary_path):
    def __init__(self, radius, points, center=None, reversed_direction=None):
        """
        builds an arc out of two endpoints and a radius.
        different ordering of endpoints give different arcs
        """
        super().__init__(points)
        self.radius = radius
        assert self.radius > 0, "0 or negative radius"
        if center is None:
            self.center = self._compute_center()
            self.reversed_direction = False  # QADH to handle reversed arcs
        else:
            # TODO: documentation
            self.center = center
            if reversed_direction is None:
                a = self.center.angle_with(self.endpoints[0]) - \
                    self.center.angle_with(self.endpoints[1])
                a = a % (2*pi)
                self.reversed_direction = False
                reversed_direction = (a > pi)
                if reversed_direction:
                    self.endpoints = list(reversed(self.endpoints))
            else:
                self.reversed_direction = reversed_direction

    def _compute_center(self):
        # take endpoints[0] as origin
        p2 = self.endpoints[1] - self.endpoints[0]
        # find bisector
        middle = p2/2
        p = middle + p2.perpendicular_vector()
        # intersect with circle at origin
        intersections = line_circle_intersections(
            [middle, p],
            point([0, 0]),
            self.radius,
            rounder2d
        )
        assert len(intersections) == 2, "invalid arc"
        # pick center and translate back
        for i in intersections:
            if p2.cross_product(i) > 0:
                return self.endpoints[0] + i
        raise "no center found"

    def get_center(self):
        return self.center

    def get_radius(self):
        return self.radius

    def reverse(self):
        copied_self = copy(self)
        copied_self.reversed_direction = not self.reversed_direction
        return copied_self

    def get_endpoints(self):
        if self.reversed_direction:
            return list(reversed(self.endpoints))
        else:
            return self.endpoints

    def get_endpoint(self, index):
        if self.reversed_direction:
            index = 1 - index
        return self.endpoints[index]

    def get_bounding_box(self):
        box = bounding_box.empty_box(2)
        box.add_point(self.center + point([self.radius, self.radius]))
        box.add_point(self.center - point([self.radius, self.radius]))
        return box

    def contains(self, p):
        """return true if point p is inside arc"""
        if p.is_almost(self.endpoints[0]) or p.is_almost(self.endpoints[1]):
            return True
        if not is_almost(self.center.distance_to(p), self.radius):
            return False
        diff = self.endpoints[1] - self.endpoints[0]
        p_diff = p - self.endpoints[0]
        product = diff.cross_product(p_diff)
        if is_almost(product, 0):
            return True
        return product < 0

    def intersections_with_arc(self, other, rounder):
        """
        intersects with an arc.
        returns up to two points.
        """
        points = circles_intersections(self.center, other.center,
                                       self.radius, other.radius, rounder)
        intersections = []
        for p in points:
            if self.contains(p) and other.contains(p):
                intersections.append(p)
        return intersections

    def intersections_with_segment(self, intersecting_segment, rounder):
        """
        intersects with a segment.
        returns up to two points.
        """
        points = line_circle_intersections(
            intersecting_segment.get_endpoints(),
            self.center,
            self.radius,
            rounder
        )
        intersections = []
        for p in points:
            if self.contains(p) and intersecting_segment.contains(p):
                intersections.append(p)

        return intersections

    def save_svg_content(self, display, color):
        x1, y1, x2, y2 = [
            c for p in self.endpoints
            for c in display.convert_coordinates(p.get_coordinates())
        ]
        r = display.stretch() * self.radius
        self.center.save_svg_content(display, color)
        display.write('<path d="M{},{} A{},{} 0 0,1 {},{}" \
                      fill="none" stroke="{}" \
                      opacity="0.5" stroke-width="3"\
                      />'.format(x1, y1, r, r, x2, y2, color))

    def vertical_intersection_at(self, x):
        """return y of lowest intersection given vertical line"""
        if self.is_vertical():
            assert x == self.endpoints[0].get_x()
            return self.lowest_endpoint().get_y()

        line = [point([x, 0]), point([x, 1])]
        intersections = \
            line_circle_intersections(line, self.center, self.radius, rounder2d)

        candidates = [i for i in intersections if self.contains(i)]
        assert candidates, "no intersection"
        ys = [i.get_y() for i in candidates]
        return min(ys)

    def distance_to_point(self, p):
        """
        returns min distance from self to point
        """
        s = segment([self.center, p])
        intersections = self.intersections_with_segment(s, rounder2d)
        if len(intersections) == 1:
            return intersections[0].distance_to(p)
        else:
            assert len(intersections) == 0
            return min([p.distance_to(q) for q in self.endpoints])

    def points_at_distance(self, p, distance):
        """
        returns from all points on self between start and end
        all which are at given distance from p.
        if end is at given distance from p only returns end
        (handles overlapping cases)
        """
        if is_almost(p.distance_to(self.endpoints[1]), distance):
            return [self.endpoints[1]]

        intersections = circles_intersections(
            self.center, p, self.radius, distance, rounder2d
        )
        remaining_points = [i for i in intersections if self.contains(i)]
        return remaining_points

    def inflate(self, radius):
        return inflate_arc(self, radius)

    def __str__(self):
        return "arc(" + str(self.radius) + ", [" + \
            str(self.endpoints[0]) + ", " + str(self.endpoints[1]) \
            + "], " + str(self.center) + ", " + str(self.reversed_direction) \
            + ")"

    def __hash__(self):
        return hash(tuple(self.endpoints)) ^ hash(self.radius) ^ \
            hash(self.reversed_direction)

    def __eq__(self, other):
        if self.endpoints != other.endpoints:
            return False
        if self.radius != other.radius:
            return False
        if self.reversed_direction != other.reversed_direction:
            return False
        return True

    def comparison(a, b):
        """
        returns if a < b.
        order has no real meaning. it is just an arbitrary order.
        precondition: both are arcs
        """
        if is_almost(a.radius, b.radius):
            if a.reversed_direction == b.reversed_direction:
                if a.endpoints[0].is_almost(b.endpoints[0]):
                    if a.endpoints[1].is_almost(a.endpoints[1]):
                        return
                    else:
                        return a.endpoints[1] < b.endpoints[1]
                else:
                    return a.endpoints[0] < b.endpoints[0]
            else:
                return a.reversed_direction
        else:
            return a.radius < b.radius

from jimn.bounding_box import bounding_box
from jimn.point import point
from jimn.segment import segment
from jimn.utils.coordinates_hash import rounder2d
from jimn.utils.math import circles_intersections, line_circle_intersections
from jimn.utils.precision import is_almost
from copy import copy
from math import pi
from jimn.tree.path_tree.path_merger import inflate_arc
from jimn.displayable import tycat

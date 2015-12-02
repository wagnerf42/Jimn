from jimn.elementary_path import elementary_path


class arc(elementary_path):
    def __init__(self, radius, points, center=None, reversed_direction=False):
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
            self.center = center
            assert is_almost(radius*radius,
                             self.center.squared_distance_to(points[0]))
            assert is_almost(radius*radius,
                             self.center.squared_distance_to(points[1]))
            self.reversed_direction = reversed_direction

    def correct_endpoints_order(self):
        """
        when creating arcs in offsetter we need to invert points
        order when distance is more than half of circle
        """
        a = self.center.angle_with(self.endpoints[0]) - \
            self.center.angle_with(self.endpoints[1])
        a = a % (2*pi)
        if a > pi:
            self.endpoints = list(reversed(self.endpoints))

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
                rounded_center = rounder2d.hash_point(self.endpoints[0] + i)
                return rounded_center
        raise "no center found"

    def get_center(self):
        return self.center

    def get_radius(self):
        return self.radius

    def reverse(self):
        copied_self = deepcopy(self)
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
        if not is_almost(self.center.squared_distance_to(p),
                         self.radius*self.radius):
            return False
        return self.contains_circle_point(p)

    def contains_circle_point(self, p):
        """returns true if point p (on circle) is inside arc"""
        if p.is_almost(self.endpoints[0]) or p.is_almost(self.endpoints[1]):
            return True
        start_angle = self.center.angle_with(self.endpoints[1])
        angles = [
            self.center.angle_with(q) - start_angle
            for q in (p, self.endpoints[0])
        ]
        adjusted_angles = []
        for a in angles:
            if a < 0:
                a += 2*pi
            adjusted_angles.append(a)
        # assert adjusted_angles[1] <= pi : TODO : fix
        return adjusted_angles[0] < adjusted_angles[1]

    def intersections_with_arc(self, other, rounder):
        """
        intersects with an arc.
        returns up to two points.
        """
        points = circles_intersections(self.center, other.center,
                                       self.radius, other.radius)
        points = [rounder2d.hash_point(p) for p in points]  # TODO: round here ?
        intersections = []
        for p in points:
            if self.contains_circle_point(p) and other.contains_circle_point(p):
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
            if self.contains_circle_point(p) \
                    and intersecting_segment.contains(p):
                intersections.append(p)

        return intersections

    def save_svg_content(self, display, color):
        x1, y1, x2, y2 = [
            c for p in self.endpoints
            for c in display.convert_coordinates(p.get_coordinates())
        ]
        r = display.stretch() * self.radius
        self.center.save_svg_content(display, color)
        stroke_width = display.stroke_width()
        display.write('<path d="M{},{} A{},{} 0 0,1 {},{}" \
                      fill="none" stroke="{}" \
                      opacity="0.5" stroke-width="{}"\
                      />'.format(x1, y1, r, r, x2, y2, color, stroke_width))

    def vertical_intersection_at(self, x):
        """return y of lowest intersection given vertical line"""
        if self.is_vertical():
            assert x == self.endpoints[0].get_x()
            return self.lowest_endpoint().get_y()

        line = [point([x, 0]), point([x, 1])]
        intersections = \
            line_circle_intersections(line, self.center, self.radius, rounder2d)

        candidates = [i for i in intersections if self.contains_circle_point(i)]
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
            self.center, p, self.radius, distance
        )
        remaining_points = [
            i for i in intersections if self.contains_circle_point(i)
        ]
        if remaining_points:
            return [rounder2d.hash_point(r) for r in remaining_points]
        else:
            if is_almost(p.distance_to(self.endpoints[0]), distance):
                return [self.endpoints[0]]
            else:
                return []

    def inflate(self, radius):
        return inflate_arc(self, radius)

    def __str__(self):
        return "arc(\n    " + str(self.radius) + ",\n    [\n        " + \
            str(self.endpoints[0]) + ",\n        " + str(self.endpoints[1]) \
            + "\n    ],\n    " + str(self.center) + ",\n    " \
            + str(self.reversed_direction) \
            + "\n)"

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

    def translate(self, translation):
        """
        translates arc by a given translation vector
        """
        return arc(self.radius, [p+translation for p in self.endpoints],
                   self.center + translation, self.reversed_direction)

from jimn.bounding_box import bounding_box
from jimn.point import point
from jimn.segment import segment
from jimn.utils.coordinates_hash import rounder2d
from jimn.utils.math import circles_intersections, line_circle_intersections
from jimn.utils.precision import is_almost
from copy import deepcopy
from math import pi
from jimn.tree.path_tree.path_merger import inflate_arc
from jimn.displayable import tycat

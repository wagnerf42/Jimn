"""
arc.
"""
from math import pi
from jimn.elementary_path import ElementaryPath
from jimn.bounding_box import BoundingBox
from jimn.point import Point
from jimn.utils.math import circles_intersections, line_circle_intersections, \
    vline_circle_intersections, compute_arc_centers
from jimn.utils.precision import is_almost
from jimn.displayable import tycat
from jimn.segment import Segment
from jimn.utils.debug import is_module_debugged


class Arc(ElementaryPath):
    """
    arc class (endpoints, radius, orientation).
    """
    def __init__(self, radius, points, center, reversed_direction=False):
        """
        build an arc out of two endpoints, a radius, a center, a direction.
        it is the user's responsibility to ensure points order is coherent
        with center and direction.
        in doubt, use "compute arc".
        """
        self.radius = radius
        self.center = center
        self.reversed_direction = reversed_direction
        super().__init__(points)
        assert self.radius > 0, "0 or negative radius"

    def adjust_center(self):
        """
        center was not completely right.
        recompute it
        """
        possible_centers = compute_arc_centers(self.radius, self.endpoints)
        distances = [
            c.distance_to(self.center) for c in possible_centers
        ]
        if distances[0] < distances[1]:
            self.center = possible_centers[0]
        else:
            self.center = possible_centers[1]

    def length(self):
        """
        return arc length.
        """
        return self.radius*self.angle()

    def angle(self):
        """
        return normalized angle of points with center.
        """
        angle = self.center.angle_with(self.endpoints[0]) - \
            self.center.angle_with(self.endpoints[1])
        angle = angle % (2*pi)
        return angle

    def correct_endpoints_order(self):
        """
        when creating arcs in offsetter we need to invert points
        order when distance is more than half of circle
        """
        if self.angle() > pi:
            self.reversed_direction = True

    def horizontal_intersections_at(self, intersecting_y, xmin, xmax):
        """
        intersections with horizontal line at given y.
        returns array of points.
        """
        segment = Segment([Point([xmin, intersecting_y]),
                           Point([xmax, intersecting_y])])
        intersections = self.intersections_with_segment(segment)
        intersections = [Point([i.get_x(), intersecting_y])
                         for i in intersections]
        return intersections

    def reverse(self):
        """
        return new arc of reversed orientation.
        """
        return Arc(self.radius, list(reversed(self.endpoints)),
                   self.center, not self.reversed_direction)

    def get_bounding_box(self):
        """
        bounding box for arc.
        for now, not tight
        """
        box = BoundingBox.empty_box(2)
        box.add_point(self.center + Point([self.radius, self.radius]))
        box.add_point(self.center - Point([self.radius, self.radius]))
        return box

    def contains(self, point):
        """return true if point is inside arc"""
        if not is_almost(self.center.distance_to(point), self.radius):
            return False
        return self.contains_circle_point(point)

    def intersections_with_arc(self, other):
        """
        intersects with an arc.
        returns up to two points.
        """
        points = circles_intersections(self.center, other.center,
                                       self.radius, other.radius)
        return [
            p for p in points
            if self.contains_circle_point(p) and
            other.contains_circle_point(p)
        ]

    def intersections_with_segment(self, intersecting_segment):
        """
        intersects with a segment.
        returns up to two points.
        """
        points = line_circle_intersections(
            intersecting_segment.endpoints,
            self.center,
            self.radius,
        )
        return [
            p for p in points
            if self.contains_circle_point(p) and
            intersecting_segment.contains(p)
        ]

    def save_svg_content(self, display, color):
        """
        svg for tycat.
        """
        # display first point to know orientation
        self.endpoints[0].save_svg_content(display, color)

        # display arc
        x_1, y_1, x_2, y_2 = [
            c for p in self.endpoints
            for c in display.convert_coordinates(p.coordinates)
        ]
        if self.reversed_direction:
            sweep_flag = 0
        else:
            sweep_flag = 1
        stretched_radius = display.svg_stretch * self.radius
        self.center.save_svg_content(display, color)
        stroke_width = display.stroke_width()

        display.write('<path d="M{},{} A{},{} 0 0,{} {},{}" \
                      fill="none" stroke="{}" \
                      opacity="0.5" stroke-width="{}"\
                      />'.format(x_1, y_1,
                                 stretched_radius, stretched_radius,
                                 sweep_flag,
                                 x_2, y_2, color, stroke_width))

    def vertical_intersection_at(self, intersecting_x):
        """return y of lowest intersection given vertical line"""
        if self.is_almost_vertical():
            assert intersecting_x == self.endpoints[0].get_x()
            return self.lowest_endpoint().get_y()

        squared_radius = self.radius * self.radius
        intersections = \
            vline_circle_intersections(intersecting_x,
                                       self.center, squared_radius)

        candidates = [i for i in intersections if self.contains_circle_point(i)]
        if __debug__ and not candidates:
            print(self, intersecting_x, [str(i) for i in intersections])
            tycat(self, intersections)
            raise Exception("no intersections")
        intersecting_ys = [i.get_y() for i in candidates]
        return min(intersecting_ys)

    def translate(self, translation):
        """
        translates arc by a given translation vector
        """
        return Arc(self.radius, [p+translation for p in self.endpoints],
                   self.center + translation, self.reversed_direction)

    def intersections_with(self, other):
        """
        return array of intersections with arc or segment.
        """
        if isinstance(other, Segment):
            intersections = self.intersections_with_segment(other)
        else:
            intersections = self.intersections_with_arc(other)

        if __debug__:
            if is_module_debugged(__name__):
                print("intersections are:")
                tycat(self, other, intersections)
        return intersections

    def contains_circle_point(self, point):
        """
        return true if point (on circle) is inside arc.
        """
        if point.is_almost(self.endpoints[0]) or \
                point.is_almost(self.endpoints[1]):
            return True
        start_angle = self.center.angle_with(self.endpoints[1])
        angles = [
            self.center.angle_with(p) - start_angle
            for p in (point, self.endpoints[0])
        ]
        adjusted_angles = []
        for angle in angles:
            if angle < 0:
                angle += 2*pi
            adjusted_angles.append(angle)
        # assert adjusted_angles[1] <= pi : TODO : fix
        if self.reversed_direction:
            return adjusted_angles[1] < adjusted_angles[0]
        else:
            return adjusted_angles[0] < adjusted_angles[1]

    def __eq__(self, other):
        if not isinstance(other, Arc):
            return False
        if self.endpoints != other.endpoints:
            return False
        if self.radius != other.radius:
            return False
        if self.reversed_direction != other.reversed_direction:
            return False
        return True

    def __str__(self):
        return "Arc(\n    " + str(self.radius) + ",\n    [\n        " + \
            str(self.endpoints[0]) + ",\n        " + str(self.endpoints[1]) \
            + "\n    ],\n    " + str(self.center) + ",\n    " \
            + str(self.reversed_direction) \
            + "\n)"

    def __hash__(self):
        return hash(tuple(self.endpoints)) ^ hash(self.radius) ^ \
            hash(self.reversed_direction)

    def __lt__(self, other):
        raise Exception("arc comparison still in use")

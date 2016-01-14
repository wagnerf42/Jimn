"""
arc segment
"""
from math import pi
from copy import deepcopy
from jimn.elementary_path import Elementary_Path
from jimn.bounding_box import Bounding_Box
from jimn.point import Point
from jimn.utils.math import circles_intersections, line_circle_intersections, \
    milling_heights, vline_circle_intersections, compute_arc_centers
from jimn.utils.precision import is_almost
from jimn.displayable import tycat
from jimn.segment import Segment


class Arc(Elementary_Path):
    """
    arc segment
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
            c.squared_distance_to(self.center) for c in possible_centers
        ]
        if distances[0] < distances[1]:
            self.center = possible_centers[0]
        else:
            self.center = possible_centers[1]

    def squared_length(self):
        """
        return square of arc length.
        """
        length = self.length()
        return length * length

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
            self.endpoints = list(reversed(self.endpoints))
            self.reversed_direction = True

    def inflate(self):
        """
        push arc towards outside by radius (double the radius)
        """
        new_radius = 2 * self.radius
        new_points = [p*2-self.center for p in self.endpoints]
        return Arc(new_radius, new_points,
                   self.center, self.reversed_direction)

    def horizontal_intersections_at(self, intersecting_y, xmin, xmax):
        """
        intersections with horizontal line at given y.
        returns array of points.
        """
        segment = Segment.horizontal_segment(xmin, xmax, intersecting_y)
        intersections = self.intersections_with_segment(segment)
        for i in intersections:
            i.set_y(intersecting_y)
        return intersections

    def split_at_milling_points(self, milling_diameter):
        """
        returns array of arcs obtained when stopping at each milling height
        """
        self.adjust_points_at_milling_height(milling_diameter)
        box = self.get_bounding_box()
        y_limits = box.limits(1)

        points = []
        for milling_y in milling_heights(*y_limits, milling_diameter,
                                         inclusive=True):
            points.extend(self.horizontal_intersections_at(milling_y,
                                                           *box.limits(0)))

        return self.split_at(points)

    def reverse(self):
        """
        return new arcs of reversed orientation
        """
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
        """
        bounding box for arc.
        for now, not tight
        """
        box = Bounding_Box.empty_box(2)
        box.add_point(self.center + Point([self.radius, self.radius]))
        box.add_point(self.center - Point([self.radius, self.radius]))
        return box

    def contains(self, point):
        """return true if point is inside arc"""
        if not is_almost(self.center.squared_distance_to(point),
                         self.radius*self.radius):
            return False
        return self.contains_circle_point(point)

    def contains_circle_point(self, point):
        """returns true if point (on circle) is inside arc"""
        if point.is_almost(self.endpoints[0]) or \
                point.is_almost(self.endpoints[1]):
            return True
        start_angle = self.center.angle_with(self.endpoints[1])
        angles = [
            self.center.angle_with(q) - start_angle
            for q in (point, self.endpoints[0])
        ]
        adjusted_angles = []
        for angle in angles:
            if angle < 0:
                angle += 2*pi
            adjusted_angles.append(angle)
        # assert adjusted_angles[1] <= pi : TODO : fix
        return adjusted_angles[0] < adjusted_angles[1]

    def intersections_with_arc(self, other):
        """
        intersects with an arc.
        returns up to two points.
        """
        points = circles_intersections(self.center, other.center,
                                       self.radius, other.radius)
        return [
            p for p in points
            if self.contains_circle_point(p)
            and other.contains_circle_point(p)
        ]

    def intersections_with_segment(self, intersecting_segment):
        """
        intersects with a segment.
        returns up to two points.
        """
        points = line_circle_intersections(
            intersecting_segment.get_endpoints(),
            self.center,
            self.radius,
        )
        return [
            p for p in points
            if self.contains_circle_point(p)
            and intersecting_segment.contains(p)
        ]

    def save_svg_content(self, display, color):
        """
        svg for tycat
        """
        # display first point to know orientation
        self.get_endpoint(0).save_svg_content(display, color)

        # display arc
        x_1, y_1, x_2, y_2 = [
            c for p in self.endpoints
            for c in display.convert_coordinates(p.get_coordinates())
        ]
        stretched_radius = display.stretch() * self.radius
        self.center.save_svg_content(display, color)
        stroke_width = display.stroke_width()
        display.write('<path d="M{},{} A{},{} 0 0,1 {},{}" \
                      fill="none" stroke="{}" \
                      opacity="0.5" stroke-width="{}"\
                      />'.format(x_1, y_1,
                                 stretched_radius, stretched_radius,
                                 x_2, y_2, color, stroke_width))

    def get_display_string(self, display):
        """
        return svg code for including arc in a svg path
        """
        end = self.get_endpoint(1)
        coordinates = display.convert_coordinates(end.coordinates)
        stretched_radius = display.stretch() * self.radius
        if self.reversed_direction:
            sweep_flag = 0
        else:
            sweep_flag = 1
        return 'A{},{} 0 0,{} {},{}'.format(stretched_radius,
                                            stretched_radius,
                                            sweep_flag,
                                            *coordinates)

    def vertical_intersection_at(self, intersecting_x):
        """return y of lowest intersection given vertical line"""
        if self.is_vertical():
            assert intersecting_x == self.endpoints[0].get_x()
            return self.lowest_endpoint().get_y()

        squared_radius = self.radius * self.radius
        intersections = \
            vline_circle_intersections(intersecting_x,
                                       self.center, squared_radius)

        candidates = [i for i in intersections if self.contains_circle_point(i)]
        if __debug__ and not candidates:
            print(self, intersecting_x, *intersections)
            tycat(self, intersections)
            raise Exception("no intersections")
        intersecting_ys = [i.get_y() for i in candidates]
        return min(intersecting_ys)

    def __str__(self):
        return "Arc(\n    " + str(self.radius) + ",\n    [\n        " + \
            str(self.endpoints[0]) + ",\n        " + str(self.endpoints[1]) \
            + "\n    ],\n    " + str(self.center) + ",\n    " \
            + str(self.reversed_direction) \
            + "\n)"

    def __hash__(self):
        return hash(tuple(self.endpoints)) ^ hash(self.radius) ^ \
            hash(self.reversed_direction)

    def equals(self, other):
        """
        full equality on all fields
        """
        if self.endpoints != other.endpoints:
            return False
        if self.radius != other.radius:
            return False
        if self.reversed_direction != other.reversed_direction:
            return False
        return True

    def comparison(self, other):
        """
        returns if self < other.
        order has no real meaning. it is just an arbitrary order.
        precondition: both are arcs
        """
        if is_almost(self.radius, other.radius):
            if self.reversed_direction == other.reversed_direction:
                if self.endpoints[0].is_almost(other.endpoints[0]):
                    if self.endpoints[1].is_almost(other.endpoints[1]):
                        return
                    else:
                        return self.endpoints[1] < other.endpoints[1]
                else:
                    return self.endpoints[0] < other.endpoints[0]
            else:
                return self.reversed_direction
        else:
            return self.radius < other.radius

    def translate(self, translation):
        """
        translates arc by a given translation vector
        """
        return Arc(self.radius, [p+translation for p in self.endpoints],
                   self.center + translation, self.reversed_direction)

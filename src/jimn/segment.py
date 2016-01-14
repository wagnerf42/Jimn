"""
segment between two points
"""
from math import pi, cos, sin
from collections import defaultdict
from jimn.elementary_path import Elementary_Path
from jimn.bounding_box import Bounding_Box
from jimn.point import Point
from jimn.utils.coordinates_hash import rounder2d, rounder_lines
from jimn.utils.precision import check_precision, is_almost
from jimn.utils.math import milling_heights
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged


class Segment(Elementary_Path):
    """
    oriented segment between two points
    """
    def __init__(self, points):
        super().__init__(points)

    def sort_endpoints(self):
        """
        sort endpoints and return a new path (same type).
        """
        return Segment(list(sorted(self.endpoints)))

    @classmethod
    def horizontal_segment(cls, xmin, xmax, y_coordinate):
        """
        constructor: creates an horizontal segment
        """
        coordinates = ([xmin, y_coordinate], [xmax, y_coordinate])
        return cls([Point(c) for c in coordinates])

    def split_at_milling_points(self, milling_diameter):
        """
        returns array of segments obtained when stopping at each milling height
        """
        self.adjust_points_at_milling_height(milling_diameter)
        y_1, y_2 = [p.get_y() for p in self.endpoints]
        points = [self.endpoints[0]]
        for intersecting_y in milling_heights(y_1, y_2, milling_diameter):
            points.append(self.horizontal_intersection_at(intersecting_y))
        points.append(self.endpoints[1])

        try:
            chunks = [
                Segment([points[i], points[i+1]])
                for i in range(len(points)-1)
            ]
        except:
            print("failed splitting", self, "for diameter", milling_diameter)
            raise
        return chunks

    def horizontal_intersection_at(self, intersecting_y):
        """
        returns point on self at given y.
        precondition : y is valid height in segment
        """
        (x_1, y_1), (x_2, y_2) = [p.get_coordinates() for p in self.endpoints]
        if is_almost(x_1, x_2):
            return Point([x_1, intersecting_y])
        else:
            slope = (y_1 - y_2) / (x_1 - x_2)
            intersecting_x = (intersecting_y - y_1) / slope + x_1
            return Point([intersecting_x, intersecting_y])

    def reverse(self):
        """invert endpoints"""
        return Segment(list(reversed(self.endpoints)))

    def __str__(self):
        return "Segment([" + str(self.endpoints[0]) + ", " + \
            str(self.endpoints[1]) + "])"

    def get_bounding_box(self):
        """
        return min bounding box containing self
        """
        boxes = [
            Bounding_Box(p.get_coordinates(), p.get_coordinates())
            for p in self.endpoints
        ]
        boxes[0].update(boxes[1])
        return boxes[0]

    def save_svg_content(self, display, color):
        """
        svg for tycat
        """
        svg_coordinates = [
            c for point in self.endpoints
            for c in display.convert_coordinates(point.get_coordinates())
        ]
        stroke_width = display.stroke_width()
        display.write("<line x1=\"{}\" y1=\"{}\"\
                      x2=\"{}\" y2=\"{}\"".format(*svg_coordinates))
        display.write(" stroke-width=\"{}\" stroke=\"{}\"\
                      opacity=\"0.5\"/>\n".format(stroke_width, color))
        # have a small arrow
        center = self.endpoints[0]*0.5 + self.endpoints[1]*0.5
        before = self.endpoints[0]*0.6 + self.endpoints[1]*0.4
        top = before.rotate_around(self.endpoints[0], pi/20)
        bottom = before.rotate_around(self.endpoints[0], -pi/20)
        top_coordinates = [
            c for point in [center, top]
            for c in display.convert_coordinates(point.get_coordinates())
        ]
        bottom_coordinates = [
            c for point in [center, bottom]
            for c in display.convert_coordinates(point.get_coordinates())
        ]
        display.write("<line x1=\"{}\" y1=\"{}\"\
                      x2=\"{}\" y2=\"{}\"".format(*top_coordinates))
        display.write(" stroke-width=\"{}\" stroke=\"{}\"\
                      opacity=\"0.5\"/>\n".format(stroke_width, color))
        display.write("<line x1=\"{}\" y1=\"{}\"\
                      x2=\"{}\" y2=\"{}\"".format(*bottom_coordinates))
        display.write(" stroke-width=\"{}\" stroke=\"{}\"\
                      opacity=\"0.5\"/>\n".format(stroke_width, color))

    def get_display_string(self, display):
        """
        return svg code for including segment in a svg path
        """
        real_coordinates = self.endpoints[1].coordinates
        coordinates = display.convert_coordinates(real_coordinates)
        return "L {},{}".format(*coordinates)

    def horizontal_plane_intersection(self, intersecting_z):
        """
        PREREQUISITE: 3d segment.
        cut it with plane at given height.
        requires h between hmin and hmax of segment
        """
        p_1, p_2 = self.endpoints
        x_1, y_1, z_1 = p_1.get_coordinates()
        x_2, y_2, z_2 = p_2.get_coordinates()

        if __debug__:
            check_precision(z_1, z_2, 'horizontal_plane_intersection')
        intersecting_x = x_1 + (intersecting_z - z_1)/(z_2 - z_1)*(x_2 - x_1)
        intersecting_y = y_1 + (intersecting_z - z_1)/(z_2 - z_1)*(y_2 - y_1)

        return rounder2d.hash_point(Point([intersecting_x, intersecting_y]))

    def is_vertical_3d(self):
        """
        is 3d segment vertical ?
        """
        x_1, x_2 = [p.get_x() for p in self.endpoints]
        y_1, y_2 = [p.get_y() for p in self.endpoints]
        if x_1 == x_2 and y_1 == y_2:
            return True
        assert not(is_almost(x_1, x_2) and is_almost(y_1, y_2)), \
            "near vertical 3d"
        return False

    def line_hash(self, rounder):
        """
        return unique id of line on which is segment
        given rounder rounds the id so nearly aligned segments will hash
        on same value
        """
        assert self.dimension() == 2, 'only works on 2d points segment'
        (x_1, y_1), (x_2, y_2) = [p.get_coordinates() for p in self.endpoints]
        if is_almost(x_1, x_2):
            key = rounder.hash_coordinate(0, x_1)
            return ':{}'.format(key)
        else:
            slope = (y_2-y_1)/(x_2-x_1)
            height_at_origin = y_1 - slope * x_1
            slope_key = rounder.hash_coordinate(1, slope)
            height_key = rounder.hash_coordinate(1, height_at_origin)
            return '{}:{}'.format(slope_key, height_key)

    def projection2d(self):
        """
        project 3d segment to 2d
        """
        return Segment([p.projection2d() for p in self.endpoints])

    def point_projection(self, projected_point):
        """
        project point on line going through self
        """
        directing_vector = self.endpoints[1] - self.endpoints[0]
        outer_vector = projected_point - self.endpoints[0]
        return self.endpoints[0] + directing_vector * \
            (outer_vector.scalar_product(directing_vector) /
             directing_vector.scalar_product(directing_vector))

    def angle(self):
        """
        return angle from origin between endpoints
        """
        return self.endpoints[0].angle_with(self.endpoints[1])

    def intersection_with_segment(self, other):
        """
        intersect two 2d segments.
        only return point if included on the two segments.
        """
        # compute point
        i = self.line_intersection_with(other)
        if i is None:
            return  # parallel lines

        # check validity
        for box in [s.get_bounding_box() for s in (self, other)]:
            if not box.almost_contains_point(i):
                return
        return i

    def line_intersection_with(self, other):
        """returns point intersecting with the two lines passing through the segments.
        None if lines are almost parallel
        """
        x1, y1, x2, y2, x3, y3, x4, y4 = [
            c for s in (self, other)
            for p in s.get_endpoints()
            for c in p.get_coordinates()
        ]
        denominator = (x1-x2) * (y4-y3) + (x3-x4) * (y1-y2)
        if is_almost(denominator, 0):
            return
        x = x1*(x3*y4-x4*y3)+x2*(x4*y3-x3*y4)+(x3-x4)*(y1*x2-x1*y2)
        x /= denominator
        y = y2*(x1*(y4-y3)-x3*y4+x4*y3) + y1*(x3*y4+x2*(y3-y4)-x4*y3)
        y /= denominator
        return Point([x, y])

    def parallel_segment(self, distance, side=1):
        """
        return segment parallel to self at given ditance,
        on given side
        """
        angle = self.endpoints[0].angle_with(self.endpoints[1])
        angle += side*pi/2
        displacement = Point([
            distance * cos(-angle),
            distance * sin(-angle)
        ])
        return Segment([p + displacement for p in self.endpoints])

    def contains(self, possible_point):
        """
        is given point inside us ?
        """
        distance = sum([possible_point.distance_to(p) for p in self.endpoints])
        return is_almost(distance,
                         self.endpoints[0].distance_to(self.endpoints[1]))

    def vertical_intersection_at(self, intersecting_x):
        """
        intersect with vertical line at given x. (return y)
        if we are a vertical segment at give x, return y with highest value
        """
        x_1, y_1 = self.endpoints[0].get_coordinates()
        x_2, y_2 = self.endpoints[1].get_coordinates()
        if x_1 == x_2:
            if not is_almost(intersecting_x, x_1):
                return None
            # when vertical, we return coordinate of lowest point
            return self.lowest_endpoint().get_y()
        if __debug__:
            check_precision(x_1, x_2, 'vertical_intersection_at')
        slope = (y_2-y_1)/(x_2-x_1)
        return y_1 + slope*(intersecting_x-x_1)

    def comparison(self, other):
        """
        returns if self < other.
        order has no real meaning. it is just an arbitrary order.
        """
        if self.endpoints[0].is_almost(other.endpoints[0]):
            if self.endpoints[1].is_almost(other.endpoints[1]):
                return
            else:
                return self.endpoints[1] < other.endpoints[1]
        else:
            return self.endpoints[0] < other.endpoints[0]

    def translate(self, translation):
        """
        translates segment by a given translation vector
        """
        return Segment([p+translation for p in self.endpoints])

    def hash_endpoints(self, rounder):
        self.endpoints = [rounder.hash_point(p) for p in self.endpoints]
        return self

    def remove_overlap_with(self, other):
        """
        if self and other overlap return array of arrays containing non common
        parts (for self and for other). if no overlap returns None
        """
        if id(self) == id(other):
            return  # abort if with self
        if self.line_hash(rounder_lines) != other.line_hash(rounder_lines):
            return  # abort if segments are not aligned

        sides = (self, other)
        events = defaultdict(list)
        for side_number, side in enumerate(sides):
            for j, p in enumerate(side.endpoints):
                events[p].append((side_number, 2*j-1))

        inside = [0, 0]
        results = [[], []]
        entered = [None, None]
        overlap = False
        for p in sorted(list(events.keys())):
            old_inside = list(inside)
            for e in events[p]:
                inside[e[0]] += e[1]

            if abs(sum(old_inside)) == 1:
                # we were on only 1 path
                for side, count in enumerate(old_inside):
                    if abs(count) == 1:
                        # we are leaving kept part
                        if count == 1:
                            results[side].append(Segment([p, entered[side]]))
                        else:
                            results[side].append(Segment([entered[side], p]))

            if abs(inside[0]) == 1 and abs(inside[1]) == 1:
                overlap = True

            for side, count in enumerate(inside):
                if abs(count) == 1:
                    entered[side] = p

        if overlap:
            return results

    def intersections_with(self, other):
        """
        return array of intersections with arc or segment.
        """
        if isinstance(other, Segment):
            i = self.intersection_with_segment(other)
            if i is None:
                intersections = []
            else:
                intersections = [i]
        else:
            intersections = other.intersections_with_segment(self)

        if __debug__:
            if is_module_debugged(__name__):
                print("intersections are:")
                tycat(self, other, intersections)

        return intersections

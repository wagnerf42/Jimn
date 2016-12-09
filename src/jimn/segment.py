"""
segment between two points.
"""
from math import pi, cos, sin
from jimn.elementary_path import ElementaryPath
from jimn.bounding_box import BoundingBox
from jimn.point import Point
from jimn.utils.coordinates_hash import LINES_ROUNDER
from jimn.utils.precision import is_almost, PRECISION
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.tour import tour
from jimn.utils.iterators import all_two_elements
from jimn.utils.coordinates_hash import ROUNDER2D


class Segment(ElementaryPath):
    """
    oriented segment between two points.

    for example:

    - create a new segment between two points:

        segment = Segment([point1, point2])

    - create a new segment from coordinates:

        segment = Segment([Point([1, 2]), Point([3, 4])])

    - compute intersection point with other segment:

        intersection = segment1.intersection_with_segment(segment2)

    """
    def __init__(self, points):
        """
        create a segment from an array of two points.
        """
        super().__init__(points)

    def copy(self):
        """
        return duplicate of given segment (no shared points with original,
        they are also copied).
        """
        return Segment([p.copy() for p in self.endpoints])

    def length(self):
        """
        return length of segment.
        example:
            segment = Segment([Point([1, 1]), Point([5, 1])])
            distance = segment.length() # distance is 4
        """
        return self.endpoints[0].distance_to(self.endpoints[1])

    def sort_endpoints(self):
        """
        return a new segment with sorted endpoints.
        """
        if self.endpoints[0] > self.endpoints[1]:
            self.endpoints = [self.endpoints[1], self.endpoints[0]]
        return self

    def horizontal_intersection_at(self, intersecting_y):
        """
        return point on self at given y.
        precondition : y is valid height in segment.
        """
        (x_1, y_1), (x_2, y_2) = [p.coordinates for p in self.endpoints]
        if is_almost(x_1, x_2):
            return Point([x_1, intersecting_y])
        else:
            slope = (y_1 - y_2) / (x_1 - x_2)
            intersecting_x = (intersecting_y - y_1) / slope + x_1
            return Point([intersecting_x, intersecting_y])

    def reverse(self):
        """
        return a new segment with inverted endpoints.
        """
        return Segment(list(reversed(self.endpoints)))

    def get_bounding_box(self):
        """
        return min bounding box containing self.
        """
        box = BoundingBox.empty_box(2)
        for point in self.endpoints:
            box.add_point(point)
        return box

    def svg_content(self):
        """
        svg for tycat.
        """
        return '<line x1="{}" y1="{}" x2="{}" y2="{}"/>\n'.format(
            *self.endpoints[0].coordinates,
            *self.endpoints[1].coordinates)

    def path_string(self):
        """
        return svg code for including segment in a svg path.
        """
        return "L {},{}".format(*self.endpoints[1].coordinates)


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

    def line_hash(self):
        """
        return unique id of line on which is segment.
        nearly aligned segments will hash
        on same value.
        """
        (x_1, y_1), (x_2, y_2) = [p.coordinates for p in self.endpoints]
        if is_almost(x_1, x_2):
            return str(LINES_ROUNDER.hash_point(Point([x_1])))
        else:
            slope = (y_2-y_1)/(x_2-x_1)
            height_at_origin = y_1 - slope * x_1
            return str(LINES_ROUNDER.hash_point(
                Point([slope, height_at_origin])))

    def projection(self, dimension):
        """
        return a new segment in smaller space (keep "dimension"
        first coordinates)
        """
        return Segment([p.projection(dimension) for p in self.endpoints])

    def point_projection(self, projected_point):
        """
        project point on line going through self.
        """
        directing_vector = self.endpoints[1] - self.endpoints[0]
        outer_vector = projected_point - self.endpoints[0]
        return self.endpoints[0] + directing_vector * \
            (outer_vector.scalar_product(directing_vector) /
             directing_vector.scalar_product(directing_vector))

    def angle(self):
        """
        return angle from origin between endpoints.
        """
        return self.endpoints[0].angle_with(self.endpoints[1])

    def intersection_with_segment(self, other):
        """
        intersect two 2d segments.
        only return point if included on the two segments.
        """
        # compute point
        if not(self < other):  # to avoid commutativity problems
            self, other = other, self

        i = self.line_intersection_with(other)
        if i is None:
            return  # parallel lines

        if self.contains(i) and other.contains(i):
            return i
        else:
            return

    def line_intersection_with(self, other):
        """
        return point intersecting with the two lines passing through
        the segments.
        none if lines are almost parallel.
        """
        # solve following system :
        # intersection = start of self + alpha * direction of self
        # intersection = start of other + beta * direction of other
        directions = [s.endpoints[1] - s.endpoints[0] for s in (self, other)]
        denominator = directions[0].cross_product(directions[1])
        if is_almost(denominator, 0):
            # parallel lines
            return
        start_diff = other.endpoints[0] - self.endpoints[0]
        alpha = start_diff.cross_product(directions[1]) / denominator
        return self.endpoints[0] + directions[0] * alpha

    def parallel_segment(self, distance, side=1):
        """
        return segment parallel to self at given distance,
        on given side. this operation might lead to precision problems.
        keep in mind that if you have three nearly aligned points, by
        taking parallel segments you might obtain four non-aligned points.
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
        # we have precisions issues here
        # we need to compare rounded distances
        # by rounding at PRECISION+1 we ensure that imprecision is less than
        # what is rounded by coordinates hashes
        return is_almost(distance,
                         self.endpoints[0].distance_to(self.endpoints[1]),
                         10**-(PRECISION+1))

    def vertical_intersection_at(self, intersecting_x):
        """
        intersect with vertical line at given x. (return y)
        if we are a vertical segment at give x, return y with highest value
        """
        x_1, y_1 = self.endpoints[0].coordinates
        x_2, y_2 = self.endpoints[1].coordinates
        if is_almost(x_1, x_2):
            raise Exception("vertical intersection with vertical segment")
            # if is_almost(intersecting_x, x_1):
            #     min_point = min(self.endpoints)
            #     return min_point.get_y()
            # else:
            #     return None
        if is_almost(intersecting_x, x_1):
            return y_1
        if is_almost(intersecting_x, x_2):
            return y_2

        slope = (y_2-y_1)/(x_2-x_1)
        intersecting_y = y_1 + slope*(intersecting_x-x_1)
        return intersecting_y

    def translate(self, translation):
        """
        translates segment by a given translation vector
        """
        return Segment([p+translation for p in self.endpoints])

    def overlaps(self, other):
        """
        return True if self and other overlap (on more than one point).
        """
        if self.line_hash() != other.line_hash():
            return False
        points = sorted([(p, id(s)) for s in (self, other)
                         for p in s.endpoints])
        if points[1][0].is_almost(points[2][0]):
            return False  # segments just touch each other
        return points[0][1] != points[1][1]

    def remove_overlap_with(self, other):
        """
        if self and other overlap return array of segments
        that are left after removal of other from self (possibly empty array).
        """

        points = list(set([p for s in (self, other)
                           for p in s.endpoints]))
        direction = self.endpoints[1] < self.endpoints[0]
        sorted_points = sorted(points, reverse=direction)
        elementary_segments = [
            Segment([a, b])
            for a, b in zip(sorted_points[:-1], sorted_points[1:])
        ]
        middle_points = [(s.endpoints[0] + s.endpoints[1])/2
                         for s in elementary_segments]
        return [s for s, m in zip(elementary_segments, middle_points)
                if not other.contains(m) and self.contains(m)]

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

    def sweeping_key(self, current_point):
        """
        return key used for comparing paths in sweeping line algorithms.
        for any given vline at x, key is current intersection on self and
        direction towards which we go (twice).
        """
        current_x = current_point.coordinates[0]
        if __debug__:
            x_coordinates = sorted([p.get_x() for p in self.endpoints])
            if not x_coordinates[0] <= current_x <= x_coordinates[1]:
                print("path is", self, "current x is:", current_x)
                raise Exception("non comparable paths in tree")

        if self.endpoints[0].coordinates[0] == self.endpoints[1].coordinates[0]:
            return (current_point, pi, pi)

        # start by finding the path's y for current x
        point_key = Point([current_x,
                           self.vertical_intersection_at(current_x)])
        point_key = ROUNDER2D.hash_point(point_key)
        point_key.coordinates[0] = current_x

        # we dont use sorted since this function is critical to performances
        if self.endpoints[0] < self.endpoints[1]:
            first_point, last_point = self.endpoints
        else:
            last_point, first_point = self.endpoints

        terminal_angle = round(
            (pi/2 - first_point.angle_with(last_point)) % pi, 10)
        # now just reverse angles based on direction
        if last_point.is_almost(point_key):
            full_key = (point_key, -terminal_angle, -terminal_angle)
        else:
            full_key = (point_key, terminal_angle, terminal_angle)
        # print("key for", self, "is", str(full_key[0]), full_key[1:])
        return full_key

    def clip(self, center, size):
        """
        clip self in square of given size at given center.
        """
        # TODO move in elementary path
        points = [
            center + Point([-size, -size]),
            center + Point([size, -size]),
            center + Point([size, size]),
            center + Point([-size, size]),
        ]
        segments = [Segment([a, b]) for a, b in all_two_elements(points)]
        intersections = []
        for segment in segments:
            intersection = segment.intersection_with_segment(self)
            if intersection:
                intersections.append(intersection)

        box = BoundingBox.empty_box(2)
        box.add_point(points[0])
        box.add_point(points[2])

        for chunk in self.split_at(intersections):
            middle = (chunk.endpoints[0] + chunk.endpoints[1])/2
            if box.almost_contains_point(middle):
                return chunk

    def __str__(self):
        return "Segment([" + str(self.endpoints[0]) + ", " + \
            str(self.endpoints[1]) + "])"

    def __eq__(self, other):
        if not isinstance(other, Segment):
            return False
        if self.endpoints != other.endpoints:
            return False
        return True

    def __hash__(self):
        return hash(tuple(self.endpoints))

    def __lt__(self, other):
        """
        return if self < other.
        order has no real meaning. it is just an arbitrary order.
        """
        return self.endpoints < other.endpoints


def __tour():
    description = "we provide a 'Segment' class encoding oriented segments."
    example = """
from jimn.point import Point
from jimn.segment import Segment
from jimn.displayable import tycat
segment1 = Segment([Point([0, 0]), Point([5, 5])])
segment2 = Segment([Point([0, 3]), Point([7, 1])])
tycat(segment1, segment2, segment1.intersection_with_segment(segment2))
    """
    tour("jimn.segment", description, example)


if __name__ == "__main__":
    __tour()

# pylint: disable=wrong-import-position
from jimn.polygon import Polygon

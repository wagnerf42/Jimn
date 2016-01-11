"""
points (2d or 3d) or vectors
"""
from math import atan2, sqrt, cos, sin, pi, floor, ceil
from jimn.bounding_box import Bounding_Box
from jimn.utils.precision import is_almost


class Point:
    """A point is defined as a vector of any given dimension."""

    def __init__(self, coordinates):
        """init a point with given coordinates"""
        self.coordinates = coordinates

    def __str__(self):
        """print code generating the point"""
        return "Point([" + ', '.join([str(c) for c in self.coordinates]) + "])"

    def squared_distance_to(self, other):
        """squared euclidean distance between two points"""
        diff = other - self
        return diff.scalar_product(diff)

    def distance_to(self, other):
        """euclidean distance between two points"""
        return sqrt(self.squared_distance_to(other))

    def dimension(self):
        """dimension of space containing the point"""
        return len(self.coordinates)

    def get_coordinates(self):
        """return the coordinates' list of the point."""
        return self.coordinates

    def get_x(self):
        """return the first coordinate, assuming it exists."""
        return self.coordinates[0]

    def get_y(self):
        """return the second coordinate, assuming it exists."""
        return self.coordinates[1]

    def get_z(self):
        """return the third coordinate, assuming it exists."""
        return self.coordinates[2]

    def set_x(self, new_coordinate):
        """
        change x coordinate
        """
        self.coordinates[0] = new_coordinate

    def set_y(self, new_coordinate):
        """
        change y coordinate
        """
        self.coordinates[1] = new_coordinate

    def is_above(self, height):
        """
        Returns a boolean indicating whether or not the point is located
        (strictly) above given height.
        Assumes the point has (at least) 3 dimensions
        """
        if is_almost(self.coordinates[2], height):
            return False
        return self.coordinates[2] > height

    def angle_with(self, other):
        """angles are computed with respect to svg orientation"""
        coordinates_differences = [
            other.coordinates[i] - self.coordinates[i]
            for i in (1, 0)
        ]
        raw_angle = -atan2(*coordinates_differences)
        if raw_angle <= 0:
            raw_angle += 2*pi
        return raw_angle

    def get_bounding_box(self):
        """
        return min bounding box containing point.
        this method is defined on any displayable object.
        """
        return Bounding_Box(self.coordinates, self.coordinates)

    def save_svg_content(self, display, color):
        """
        svg display for tycat
        """
        svg_coordinates = display.convert_coordinates(self.coordinates)
        stroke_width = display.stroke_width()
        display.write("<circle cx=\"{}\" cy=\"{}\"".format(*svg_coordinates))
        display.write(" r=\"{}\" fill=\"{}\" opacity=\"0.5\"/>\n".format(
            stroke_width, color))

    def rotate(self, angle):
        """rotates a point around origin"""
        if __debug__:
            assert self.dimension() == 2, "2d rotation only"
        cosinus = cos(-angle)
        sinus = sin(-angle)
        return Point([
            self.scalar_product(Point([cosinus, -sinus])),
            self.scalar_product(Point([sinus, cosinus]))
        ])

    def rotate_around(self, center, angle):
        """rotates a point around another one"""
        return center + (self-center).rotate(angle)

    def is_aligned_with(self, point2, point3):
        """
        are the three given points approximately aligned ?
        """
        (x_1, y_1), (x_2, y_2), (x_3, y_3) = [
            p.get_coordinates() for p in (self, point2, point3)
        ]
        determinant = x_1*y_2 + y_1*x_3 + x_2*y_3 \
            - (y_2*x_3 + y_1*x_2 + x_1*y_3)
        return abs(determinant) < 10**-5  # TODO: why 5 ???

    def is_near(self, other, limit):
        """
        return true if distance from self to other
        is less than limit
        """
        distance = self.distance_to(other)
        if is_almost(distance, limit):
            return True
        return distance < limit

    def is_almost(self, other):
        """
        return true if self and other are almost the same points
        (see jimn.utils.precision)
        """
        for c_1, c_2 in zip(self.coordinates, other.coordinates):
            if not is_almost(c_1, c_2):
                return False
        return True

    def projection2d(self):
        """
        Returns the projection of the point on the first 2 dimensions.
        Assumes the point has at least 2 dimensions.
        """
        return Point(self.coordinates[0:2])

    def scalar_product(self, other):
        """
        scalar (dot) product between two vectors
        """
        return sum([c1*c2
                    for c1, c2 in zip(self.coordinates, other.coordinates)])

    def cross_product(self, other):
        """
        cross product between 2 2d vectors
        """
        x_1, y_1 = self.get_coordinates()
        x_2, y_2 = other.get_coordinates()
        return x_1 * y_2 - y_1 * x_2

    def nearest_points(self, other):
        """
        dummy procedure used in graphs.
        re-defined for other more complex classes.
        """
        return (self, other)

    def adjust_at_milling_height(self, milling_diameter):
        """
        if point is close enough from a milling height, return new point
        exactly at milling height.
        else return point
        """
        point_y = self.get_y()
        above_height = milling_diameter * floor(point_y/milling_diameter)
        below_height = milling_diameter * ceil(point_y/milling_diameter)
        for height in (above_height, below_height):
            if is_almost(point_y, height):
                return Point([self.get_x(), height])

        return self

    def perpendicular_vector(self):
        "return a vector perpendicular to given one"
        return Point([-self.coordinates[1], self.coordinates[0]])

    def __add__(self, other):
        """
        addition operator. (useful for translations)
        """
        return Point([i + j
                      for i, j in zip(self.coordinates, other.coordinates)])

    def __sub__(self, other):
        """
        substraction operator. (useful for translations)
        """
        return Point([i - j
                      for i, j in zip(self.coordinates, other.coordinates)])

    def __mul__(self, factor):
        """
        multiplication by scalar operator. (useful for scaling)
        """
        return Point([c*factor for c in self.coordinates])

    def __truediv__(self, factor):
        """
        division by scalar operator. (useful for scaling)
        """
        return Point([c/factor for c in self.coordinates])

    def __eq__(self, other):
        """
        strict equality operator
        """
        return self.coordinates == other.coordinates

    def __hash__(self):
        return hash(tuple(self.coordinates))

    def __lt__(self, other):
        """
        Returns whether or not a < b in lexicographical order.
        Assumes a and b have the same dimension.
        """
        return self.coordinates < other.coordinates

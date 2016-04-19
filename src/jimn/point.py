"""
points (2d or 3d) or vectors.
"""
from math import atan2, sqrt, cos, sin, pi
from jimn.bounding_box import BoundingBox
from jimn.utils.precision import is_almost
from jimn.utils.tour import tour


class Point:
    """
    a point is defined as a vector of any given dimension.

    for example:

    - create a point at x=2, y=5:

    my_point = Point([2, 5])

    - find distance between two points:

    distance = point1.distance_to(point2)

    - translate point by (2, 3):

    point += Point([2, 3])
    """
    def __init__(self, coordinates):
        """
        build new point using an array of coordinates.
        """
        self.coordinates = coordinates

    def distance_to(self, other):
        """
        euclidean distance between two points.
        """
        # this is not so elegant but fastest version
        # we need it to be fast since it is called many times
        total = 0
        for c_1, c_2 in zip(self.coordinates, other.coordinates):
            diff = c_1 - c_2
            total += diff * diff
        return sqrt(total)

    def get_x(self):
        """return the first coordinate, assuming it exists."""
        return self.coordinates[0]

    def get_y(self):
        """return the second coordinate, assuming it exists."""
        return self.coordinates[1]

    def get_z(self):
        """return the third coordinate, assuming it exists."""
        return self.coordinates[2]

    def angle_with(self, other):
        """
        angle formed by two points and horizontal line
        passing through first point.
        angles are computed with respect to svg orientation.
        """
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
        return BoundingBox(self.coordinates, self.coordinates)

    def save_svg_content(self, display, color):
        """
        svg display for tycat.
        """
        svg_coordinates = display.convert_coordinates(self.coordinates)
        stroke_width = 2*display.stroke_width()
        display.write("<circle cx=\"{}\" cy=\"{}\"".format(*svg_coordinates))
        display.write(" r=\"{}\" fill=\"{}\" opacity=\"0.5\"/>\n".format(
            stroke_width, color))

    def rotate(self, angle):
        """
        rotate a 2d point around origin.
        """
        cosinus = cos(-angle)
        sinus = sin(-angle)
        return Point([
            self.scalar_product(Point([cosinus, -sinus])),
            self.scalar_product(Point([sinus, cosinus]))
        ])

    def rotate_around(self, center, angle):
        """
        rotate a point around another one.
        """
        return center + (self-center).rotate(angle)

    def is_aligned_with(self, point2, point3):
        """
        are the three given points approximately aligned ?
        """
        (x_1, y_1), (x_2, y_2), (x_3, y_3) = [
            p.coordinates for p in (self, point2, point3)
        ]
        determinant = x_1*y_2 + y_1*x_3 + x_2*y_3 \
            - (y_2*x_3 + y_1*x_2 + x_1*y_3)
        return abs(determinant) < 10**-5  # TODO: why 5 ???

    def is_almost(self, other):
        """
        return true if self and other are almost the same points
        (see jimn.utils.precision)
        """
        for c_1, c_2 in zip(self.coordinates, other.coordinates):
            if not is_almost(c_1, c_2):
                return False
        return True

    def projection(self, dimension):
        """
        return the projection of the point on the first "dimension" coordinates.
        precondition : point has at least "dimension" coordinates.
        """
        return Point(self.coordinates[0:dimension])

    def scalar_product(self, other):
        """
        scalar (dot) product between two vectors
        """
        return sum([c1*c2
                    for c1, c2 in zip(self.coordinates, other.coordinates)])

    def cross_product(self, other):
        """
        cross product between 2 2d vectors.
        """
        return self.perpendicular_vector().scalar_product(other)

    def nearest_points(self, other):
        """
        dummy procedure used in graphs.
        re-defined for other more complex classes.
        """
        return (self, other)

    def perpendicular_vector(self):
        """
        return a vector perpendicular to given one.
        """
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

    def __le__(self, other):
        return self.coordinates <= other.coordinates

    def __ge__(self, other):
        return self.coordinates >= other.coordinates

    def __str__(self):
        """
        print code generating the point.
        """
        return "Point([" + ', '.join([str(c) for c in self.coordinates]) + "])"


def __tour():
    """
    give a few examples.
    """
    description = "we provide a 'Point' class encoding points or vectors."
    example = """
from jimn.point import Point
from jimn.displayable import tycat
point1 = Point([0, 0])
point2 = Point([2, 3])
tycat(point1, point2)
print(\"distance between points:\", point1.distance_to(point2))
vector = Point([1, 1])
translations = (point1 + vector, point1 - vector)
tycat(point1, point2, translations)
    """

    tour("jimn.point", description, example)


if __name__ == "__main__":
    __tour()

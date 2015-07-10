# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from math import atan2, sqrt, cos, sin
from jimn.precision import is_almost
from jimn.bounding_box import bounding_box


class point:
    """A point is defined as a vector of any given dimension."""

    def __init__(self, coordinates):
        """Inits a point with given coordinates"""
        self.coordinates = coordinates

    def __str__(self):
        """Prints "(x, y, ...)"."""
        return "({})".format(','.join(map(lambda x: str(x), self.coordinates)))

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
        """Return the coordinates' list of the point. Allow modifications."""
        return self.coordinates

    def get_x(self):
        """Return the first coordinate, assuming it exists."""
        return self.coordinates[0]

    def get_y(self):
        """Return the second coordinate, assuming it exists."""
        return self.coordinates[1]

    def get_z(self):
        """Return the third coordinate, assuming it exists."""
        return self.coordinates[2]

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
        (x1, y1), (x2, y2) = [p.get_coordinates() for p in (self, other)]
        return -atan2(y2 - y1, x2 - x1)

    def get_bounding_box(self):
        return bounding_box(self.coordinates, self.coordinates)

    # careful : only works on 2d points
    def save_svg_content(self, display, color):
        svg_coordinates = display.convert_coordinates(self.coordinates)
        display.write("<circle cx=\"{}\" cy=\"{}\"".format(*svg_coordinates))
        display.write(" r=\"5\" fill=\"{}\"/> opacity=\"0.5\"\n".format(color))

    def rotate(self, angle):
        """rotates a point around origin"""
        if __debug__:
            assert self.dimension() == 2, "2d rotation only"
        x, y = self.coordinates
        c = cos(-angle)
        s = sin(-angle)
        return point([
            c*x - s*y,
            s*x + c*y
        ])

    def rotate_around(self, center, angle):
        """rotates a point around another one"""
        return center + (self-center).rotate(angle)

    def is_aligned_with(self, p2, p3):
        (x1, y1), (x2, y2), (x3, y3) = [
            p.get_coordinates() for p in (self, p2, p3)
        ]
        determinant = x1*y2 + y1*x3 + x2*y3 - (y2*x3 + y1*x2 + x1*y3)
        return is_almost(determinant, 0)

    def is_near(self, other, limit):
        distance = self.distance_to(other)
        if is_almost(distance, limit):
            return True
        return distance < limit

    def is_almost(self, other):
        assert self.dimension() == other.dimension(), "comparing different points"
        for u1, u2 in zip(self.coordinates, other.coordinates):
            if not is_almost(u1, u2):
                return False
        return True

    def projection2d(self):
        """
        Returns the projection of the point on the first 2 dimensions.
        Assumes the point has at least 2 dimensions.
        """
        x, y = self.coordinates[0:2]
        return point([x, y])

    def scalar_product(self, other):
        p = 0
        for c1, c2 in zip(self.coordinates, other.coordinates):
            p += c1 * c2
        return p

    def is_on_slice(self, milling_diameter):
        d = milling_diameter
        y = self.get_y()
        return is_almost(y/d, round(y/d))

    def __add__(a, b):
        return point([i + j for i, j in zip(a.coordinates, b.coordinates)])

    def __sub__(a, b):
        return point([i - j for i, j in zip(a.coordinates, b.coordinates)])

    def __truediv__(a, i):
        return point([c/i for c in a.coordinates])

    def __eq__(a, b):
        return a.coordinates == b.coordinates

    def __hash__(self):
        return hash(tuple(self.coordinates))

    def __lt__(a, b):
        """
        Returns whether or not a < b in lexicographical order.
        Assumes a and b have the same dimension.
        """
        return a.coordinates < b.coordinates

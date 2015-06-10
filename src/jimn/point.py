# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from math import atan2
from jimn.precision import is_almost


class point:
    """A point is defined as a vector of any given dimension."""

    def __init__(self, coordinates):
        """Inits a point whose dimension is the number of arguments, and whose coordinates are the arguments."""
        self.coordinates = coordinates

    def __str__(self):
        """Prints "(x, y, ...)"."""
        return "({})".format(','.join(map(lambda x: str(x), self.coordinates)))

    def dimension(self):
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
        Returns a boolean indicating whether or not the point is located (strictly) above z = height.
        Assumes the point has (at least) 3 dimensions
        """
        return self.coordinates[2] > height

    def angle_with(self, other):
        (x1, y1), (x2, y2) = [p.get_coordinates() for p in (self, other)]
        return atan2(y2 - y1, x2 - x1)

    def get_bounding_box(self):
        return [self.coordinates, self.coordinates]

    # careful : only works on 2d points
    def save_svg_content(self, display, color):
        svg_coordinates = display.convert_coordinates(self.coordinates)
        display.write("<circle cx=\"{}\" cy=\"{}\"".format(*svg_coordinates))
        display.write(" r=\"5\" fill=\"{}\"/> opacity=\"0.5\"\n".format(color))

    def is_aligned_with(self, p2, p3):
        (x1, y1), (x2, y2), (x3, y3) = [p.get_coordinates() for p in (self, p2, p3)]
        determinant = x1*y2 + y1*x3 +x2*y3 - (y2*x3 + y1*x2 + x1*y3)
        return is_almost(determinant, 0)

    def projection2d(self):
        """
        Returns the projection of the point on the first 2 dimensions.
        Assumes the point has at least 2 dimensions.
        """
        x, y = self.coordinates[0:2]
        return point([x, y])

    def __eq__(a, b):
        return a.coordinates == b.coordinates

    def __hash__(self):
        return hash(tuple(self.coordinates))

    def __lt__(a, b):
        """
        Returns whether or not a < b in lexicographical order.
        Assumes a and b have the same dimension.
        """
        for ca, cb in zip(a.coordinates, b.coordinates):
            if ca < cb:
                return 1
            elif ca > cb:
                return 0
        return 0

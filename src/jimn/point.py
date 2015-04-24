# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4


class point:
    """A point is defined as a vector of any given dimension."""

    def __init__(self, *arg):
        """Inits a point whose dimension is the number of arguments, and whose coordinates are the arguments."""
        self.coordinates = list(arg)

    def __str__(self):
        """Prints "(x, y, ...)"."""
        return "({})".format(','.join(map(lambda x: str(x), self.coordinates)))

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

    def get_bounding_box(self):
        return [self.coordinates, self.coordinates]

    # careful : only works on 2d points
    def save_svg_content(self, display, color):
        svg_coordinates = display.convert_coordinates(self.coordinates)
        display.write("<circle cx=\"{}\" cy=\"{}\"".format(*svg_coordinates))
        display.write(" r=\"5\" fill=\"{}\"/> opacity=\"0.5\"\n".format(color))

    def projection2d(self):
        """
        Returns the projection of the point on the first 2 dimensions.
        Assumes the point has at least 2 dimensions.
        """
        x, y = self.coordinates[0:2]
        return point(x, y)

    def __key(self):
        return tuple(self.coordinates)

    def __eq__(x, y):
        return x.__key() == y.__key()

    def __hash__(self):
        return hash(self.__key())

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

# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4


class point:

    def __init__(self, *arg):
        self.coordinates = [x for x in arg]

    def __str__(self):
        return "({})".format(','.join(map(lambda x: str(x), self.coordinates)))

    def get_coordinates(self):
        return self.coordinates

    def get_x(self):
        return self.coordinates[0]

    def get_y(self):
        return self.coordinates[1]

    def get_z(self):
        return self.coordinates[2]

    def is_above(self, height):
        return self.coordinates[2] > height

    def get_bounding_box(self):
        return [self.coordinates, self.coordinates]

    # careful : only works on 2d points
    def save_svg_content(self, display, color):
        svg_coordinates = display.convert_coordinates(self.coordinates)
        display.write("<circle cx=\"{}\" cy=\"{}\"".format(*svg_coordinates))
        display.write(" r=\"5\" fill=\"{}\"/> opacity=\"0.5\"\n".format(color))

    def projection2d(self):
        x, y = self.coordinates[0:2]
        return point(x, y)

    def __key(self):
        return tuple(self.coordinates)

    def __eq__(x, y):
        return x.__key() == y.__key()

    def __hash__(self):
        return hash(self.__key())

    def __lt__(a, b):
        for ca, cb in zip(a.coordinates, b.coordinates):
            if ca < cb:
                return 1
            elif ca > cb:
                return 0
        return 0

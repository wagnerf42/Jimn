# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
import sys
from jimn.point import point


class segment:
    def __init__(self, *points):
        self.endpoints = [p for p in points]

    def __str__(self):
        return "[{}]".format(';'.join(map(lambda p: str(p), self.endpoints)))

    def get_bounding_box(self):
        min_coordinates = [sys.float_info.max for i in self.endpoints[0].get_coordinates()]
        max_coordinates = [-sys.float_info.max for i in self.endpoints[0].get_coordinates()]
        for p in self.endpoints:
            coordinates = p.get_coordinates()
            for coordinate_index, coordinate in enumerate(coordinates):
                if coordinate < min_coordinates[coordinate_index]:
                    min_coordinates[coordinate_index] = coordinate
                if coordinate > max_coordinates[coordinate_index]:
                    max_coordinates[coordinate_index] = coordinate
        return (min_coordinates, max_coordinates)

    def save_svg_content(self, display, color):
        svg_coordinates = [c for point in self.endpoints for c in display.convert_coordinates(point.get_coordinates())]
        display.write("<line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\" stroke-width=\"3\" stroke=\"{color_arg}\"/>\n".format(*svg_coordinates, color_arg=color))

    def intersect(self, h):
        p1, p2 = self.endpoints
        x1, y1, z1 = p1.get_coordinates()
        x2, y2, z2 = p2.get_coordinates()

        z = h
        x = x1 + (z - z1)/(z2 - z1)*(x2 - x1)
        y = y1 + (z - z1)/(z2 - z1)*(y2 - y1)

        return point(x, y, z)

    def projection2d(self):
        p1, p2 = self.endpoints
        return segment(p1.projection2d(), p2.projection2d())

    def get_endpoints(self):
        return self.endpoints

    def __key(self):
        return tuple(self.endpoints)

    def __eq__(x, y):
        return x.__key() == y.__key()

    def __hash__(self):
        return hash(self.__key())

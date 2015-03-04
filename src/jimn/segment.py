# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
import sys
import operator

class segment:
    def __init__(self, p1, p2):
        self.endpoints = [p1, p2]

    def __str__(self):
        return "[{} ; {}]".format(str(self.endpoints[0]), str(self.endpoints[1]))

    def get_bounding_box(self):
        min_coordinates = [ sys.float_info.max for i in self.endpoints[0].get_coordinates() ]
        max_coordinates = [ -sys.float_info.max for i in self.endpoints[0].get_coordinates() ]
        for point in self.endpoints:
            coordinates = point.get_coordinates()
            for coordinate_index, coordinate in enumerate(coordinates):
                if coordinate < min_coordinates[coordinate_index]:
                    min_coordinates[coordinate_index] = coordinate
                if coordinate > max_coordinates[coordinate_index]:
                    max_coordinates[coordinate_index] = coordinate
        return (min_coordinates, max_coordinates)

    def save_svg_content(self, display, color):
        svg_coordinates = [ c for point in self.endpoints for c in display.convert_coordinates(point.get_coordinates()) ]
        display.write("<line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\" stroke-width=\"3\" stroke=\"{color_arg}\"/>\n".format(*svg_coordinates, color_arg=color))

    def intersect(self, h):
        p1 = self.endpoints[0]
        x1 = p1.coordinates[0]
        y1 = p1.coordinates[1]
        z1 = p1.coordinates[2]

        p2 = self.endpoints[1]
        x2 = p2.coordinates[0]
        y2 = p2.coordinates[1]
        z2 = p2.coordinates[2]

        z = h;
        x = x1 + (z - z1)/(z2 - z1)*(x2 - x1);
        y = y1 + (z - z1)/(z2 - z1)*(y2 - y1);

        return x, y, z;

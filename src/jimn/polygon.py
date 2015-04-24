# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
import sys


class polygon:

    def __init__(self, *points):
        self.endpoints = list(points)

    def append(self, point):
        self.endpoints.append(point)

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
        print(self)
        svg_coordinates = []
        for point in self.endpoints:
            string = "{},{}".format(*display.convert_coordinates(point.get_coordinates()))
            svg_coordinates.append(string)
        print(svg_coordinates)
        svg_formatted = " ".join(svg_coordinates)
        display.write("<polygon points=\"{}\"".format(svg_formatted))
        print("<polygon points=\"{}\"".format(svg_formatted))
        #display.write(" stroke-width=\"3\" stroke=\"{}\" opacity=\"0.5\" fill=lime/>\n".format(color))
        #print(" stroke-width=\"3\" stroke=\"{}\" opacity=\"0.5\" fill=lime/>\n".format(color))
        display.write(" style=\"fill:{};stroke:{};stroke-width:1\" />".format(color, color))
        print(" style=\"fill:{};stroke:{};stroke-width:1\" />".format(color, color))


    def get_endpoints(self):
        return self.endpoints

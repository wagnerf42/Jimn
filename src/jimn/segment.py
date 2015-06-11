# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
import sys
from jimn.point import point
from math import atan2
from jimn.coordinates_hash import coordinates_hash
from jimn.precision import is_almost, squared_limit

rounding_hash = coordinates_hash()

class segment:
    """A segment is defined as a set of two points"""

    def __init__(self, points):
        self.endpoints = points
        assert self.endpoints[0].dimension() == self.endpoints[1].dimension()
        if __debug__:
            if(self.squared_length() <= squared_limit):
                print("very small segment {}".format(str(self)), file=sys.stderr)
                # raise Exception("very small segment")

    def __str__(self):
        return "[{}]".format(';'.join(map(lambda p: str(p), self.endpoints)))

    def squared_length(self):
        coordinates = [p.get_coordinates() for p in self.endpoints]
        distance = 0
        for i in range(len(coordinates[0])):
            diff = coordinates[0][i] - coordinates[1][i]
            distance = distance + diff * diff
        return distance

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
        display.write("<line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\"".format(*svg_coordinates))
        display.write(" stroke-width=\"3\" stroke=\"{}\" opacity=\"0.5\"/>\n".format(color))

    def smallest_point(self):
        return min(*self.endpoints)

    def horizontal_plane_intersection(self, h):
        assert self.dimension() == 3
        p1, p2 = self.endpoints
        x1, y1, z1 = p1.get_coordinates()
        x2, y2, z2 = p2.get_coordinates()

        if __debug__:
            if is_almost(z1, z2):
                print('warning: potential precision problem', file=sys.stderr)
        z = h
        x = x1 + (z - z1)/(z2 - z1)*(x2 - x1)
        y = y1 + (z - z1)/(z2 - z1)*(y2 - y1)

        return rounding_hash.hash_point(point([x, y, z]))

    def dimension(self):
        return self.endpoints[0].dimension()

    # return unique id of line on which is segment
    def line_hash(self, rounder):
        assert self.dimension() == 2, 'only works on 2d points segment'
        (x1, y1), (x2, y2) = [ p.get_coordinates() for p in self.endpoints]
        if x1 == x2:
            return ':{}'.format(str(x1))
        else:
            if __debug__:
                if is_almost(x1, x2):
                    print('potential precision problem')
            a = (y2-y1)/(x2-x1) # TODO : handle precision problem
            b = y1 - a * x1
            return '{}:{}'.format(str(a), str(b)) # TODO: what precision ?

    def projection2d(self):
        p1, p2 = self.endpoints
        return segment([p1.projection2d(), p2.projection2d()])

    def get_endpoint(self, index):
        return self.endpoints[index]

    def get_endpoints(self):
        return self.endpoints

    def sort_endpoints(self):
        sorted_endpoints = sorted(self.endpoints, key=lambda p: (p.get_x(), p.get_y()))
        return segment(sorted_endpoints)

    def is_sorted(self):
        sorted_self = self.sort_endpoints()
        return sorted_self.endpoints == self.endpoints

    def angle(self):
        return self.endpoints[0].angle_with(self.endpoints[1])

    def __eq__(a, b):
        return a.endpoints == b.endpoints

    def __hash__(self):
        return hash(tuple(self.endpoints))

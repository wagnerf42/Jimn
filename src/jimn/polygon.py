# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.segment import segment
from jimn.polygonsegment import polygonsegment
from jimn.bounding_box import bounding_box
from jimn.precision import is_almost


class invalid_polygon(Exception):
        pass


class polygon:

    def __init__(self, points, label=None):
        if __debug__:
            if len(points) <= 2:
                raise invalid_polygon("not enough points")
        self.points = points
        if label is None:
            self.label = id(self)
        else:
            self.label = label

    def points_number(self):
        return len(self.points)

    """when 3 consecutive points are aligned the middle one is useless.
    we remove here all useless points in order to decrease cost of storage and
    computations of further operations.
    WARNING : this operation reverses orientation
    """
    def remove_useless_points(self):
        p1 = self.points.pop()
        start_point = p1
        p2 = self.points.pop()
        remaining_points = [p1]
        for p in reversed(self.points):
            if not p1.is_aligned_with(p2, p):
                remaining_points.append(p2)
                p1 = p2
            else:
                try:
                    d1 = segment([p1, p2]).squared_length()
                    d2 = segment([p1, p]).squared_length()
                except:
                    raise invalid_polygon("coming back")
                if d1 > d2:
                    raise invalid_polygon("coming back 2")
            p2 = p
        if not p1.is_aligned_with(p2, start_point):
            remaining_points.append(p2)

        self.points = remaining_points
        if __debug__:
            if len(self.points) <= 2:
                raise invalid_polygon("not enough points after simplifying")

    def get_points(self):
        return self.points

    # TODO: iterator
    def segments(self):
        s = []
        for p1, p2 in zip(self.points, self.points[1:]):
            s.append(segment([p1, p2]))
        s.append(segment([self.points[-1], self.points[0]]))
        return s

    def non_vertical_segments(self, height):
        s = []
        for p1, p2 in zip(self.points, self.points[1:]):
            seg = polygonsegment([p1, p2], height, self)
            if not seg.is_vertical():
                s.append(seg)
        seg = polygonsegment([self.points[-1], self.points[0]], height, self)
        if not seg.is_vertical():
            s.append(seg)
        return s

    def area(self):
        a = 0
        for s in self.segments():
            (x1, y1), (x2, y2) = [p.get_coordinates() for p in s.get_endpoints()]
            a = a + x1*y2 - x2*y1
        return a/2

    """clockwise is defined respectively to svg displayed"""
    def is_oriented_clockwise(self):
        a = self.area()
        assert not is_almost(a, 0), "flat polygon"
        return a > 0

    def orient(self, clockwise=True):
        if self.is_oriented_clockwise() != clockwise:
            self.points.reverse()

    def normalize_starting_point(self):
        smallest_point = self.points[0]
        index = 0
        for k, p in enumerate(self.points):
            if p < smallest_point:
                smallest_point = p
                index = k
        self.points = self.points[index:] + self.points[:index]

    # assumes the two polygons are normalized
    def is_translated(self, p2):
        translation_vector = None
        for a1, a2 in zip(self.get_points(), p2.get_points()):
            if translation_vector is None:
                translation_vector = a2 - a1
            else:
                if not (a2 - a1).is_almost(translation_vector):
                    return False
        return True

    def __str__(self):
        return "[{}/{}]".format(self.label,';'.join(map(lambda p: str(p), self.points)))

    # TODO : use new system
    def get_bounding_box(self):
        min_coordinates = [float('+inf') for i in self.points[0].get_coordinates()]
        max_coordinates = [float('-inf') for i in self.points[0].get_coordinates()]
        for p in self.points:
            coordinates = p.get_coordinates()
            for coordinate_index, coordinate in enumerate(coordinates):
                if coordinate < min_coordinates[coordinate_index]:
                    min_coordinates[coordinate_index] = coordinate
                if coordinate > max_coordinates[coordinate_index]:
                    max_coordinates[coordinate_index] = coordinate
        return bounding_box(min_coordinates, max_coordinates)

    def save_svg_content(self, display, color):
        svg_coordinates = []
        for p in self.points:
            string = "{},{}".format(*display.convert_coordinates(p.get_coordinates()))
            svg_coordinates.append(string)
        svg_formatted = " ".join(svg_coordinates)
        display.write("<polygon points=\"{}\"".format(svg_formatted))
        display.write(" style=\"fill:{};stroke:{};stroke-width:1;opacity:0.4\" />".format(color, color))

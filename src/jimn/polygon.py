# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
import sys
from jimn.segment import segment
from jimn.displayable import tycat


class polygon:

    def __init__(self, points):
        assert len(points) > 2, 'not enough points in polygon creation'
        p1 = points.pop()
        start_point = p1
        p2 = points.pop()
        self.points = [p1]
        for p in reversed(points):
            if not p1.is_aligned_with(p2, p):
                self.points.append(p2)
                p1 = p2
            p2 = p
        if not p1.is_aligned_with(p2, start_point):
            self.points.append(p2)
        assert len(self.points) > 2, 'flat polygon created'

    # TODO: have a nice iterator
    def segments(self):
        s = []
        for p1, p2 in zip(self.points, self.points[1:]):
            s.append(segment([p1, p2]))
        s.append(segment([self.points[-1], self.points[0]]))
        return s

    def orientation(self):
        a = 0
        for s in self.segments():
            (x1, y1), (x2, y2) = [p.get_coordinates() for p in s.get_endpoints()]
            a = a + x1*y1 - x2*y1
        return a

    def __str__(self):
        return "[{}]".format(';'.join(map(lambda p: str(p), self.points)))

    def get_bounding_box(self):
        min_coordinates = [sys.float_info.max for i in self.points[0].get_coordinates()]
        max_coordinates = [-sys.float_info.max for i in self.points[0].get_coordinates()]
        for p in self.points:
            coordinates = p.get_coordinates()
            for coordinate_index, coordinate in enumerate(coordinates):
                if coordinate < min_coordinates[coordinate_index]:
                    min_coordinates[coordinate_index] = coordinate
                if coordinate > max_coordinates[coordinate_index]:
                    max_coordinates[coordinate_index] = coordinate
        return (min_coordinates, max_coordinates)

    def save_svg_content(self, display, color):
        svg_coordinates = []
        for point in self.points:
            string = "{},{}".format(*display.convert_coordinates(point.get_coordinates()))
            svg_coordinates.append(string)
        svg_formatted = " ".join(svg_coordinates)
        display.write("<polygon points=\"{}\"".format(svg_formatted))
        display.write(" style=\"fill:{};stroke:{};stroke-width:1;opacity:0.4\" />".format(color, color))

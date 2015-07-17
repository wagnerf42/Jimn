# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.segment import segment
from jimn.polygonsegment import polygonsegment
from jimn.bounding_box import bounding_box
from jimn.precision import is_almost
from jimn.iterators import all_two_elements
from jimn.ghost import ghost
from math import floor, ceil


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

    def non_vertical_segments(self, height):
        return filter(lambda s: not s.is_vertical(), [
            polygonsegment([p1, p2], height, self)
            for p1, p2 in all_two_elements(self.points)]
        )

    def segments(self):
        for p1, p2 in all_two_elements(self.points):
            yield segment([p1, p2])

    def area(self):
        a = 0
        for p1, p2 in all_two_elements(self.points):
            (x1, y1), (x2, y2) = [p.get_coordinates() for p in (p1, p2)]
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

    def round_points(self, rounder):
        self.points = [rounder.hash_point(p) for p in self.points]

    def milling_heights(self, milling_diameter):
        box = self.get_bounding_box()
        ymin, ymax = box.limits(1)
        start = floor(ymin / milling_diameter)
        end = ceil(ymax / milling_diameter)
        for i in range(start, end+1):
            yield i * milling_diameter

    def create_vertices(self, milling_diameter, built_graph):
        # first cut by horizontal lines spaced by milling_diameter
        box = self.get_bounding_box()
        xmin, xmax = box.limits(0)
        cutting_lines = [
            segment.horizontal_segment(xmin, xmax, y)
            for y in self.milling_heights(milling_diameter)
        ]
        cutter = ghost(list(self.segments()))
        elementary_segments = cutter.compute_elementary_paths(cutting_lines)
        # ok, now create graph, each segment point becomes a vertex
        # and we add all external edges
        for s in elementary_segments:
            v = built_graph.add_vertex(s.get_endpoint(0))
            v.add_edge(s)
            v = built_graph.add_vertex(s.get_endpoint(1))
            v.add_edge(s.reverse())

    def __str__(self):
        return "[{}/{}]".format(
            self.label, ';'.join(map(lambda p: str(p), self.points))
        )

    def get_bounding_box(self):
        box = bounding_box.empty_box(2)
        for p in self.points:
            box.add_point(p)
        return box

    def save_svg_content(self, display, color):
        svg_coordinates = []
        for p in self.points:
            string = "{},{}".format(
                *display.convert_coordinates(p.get_coordinates())
            )
            svg_coordinates.append(string)
        svg_formatted = " ".join(svg_coordinates)
        display.write("<polygon points=\"{}\"".format(svg_formatted))
        display.write(" style=\"fill:{};stroke:{};\
                      stroke-width:1;opacity:0.4\" />".format(color, color))

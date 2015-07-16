# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.segment import segment
from jimn.vertex import vertex
from jimn.polygonsegment import polygonsegment
from jimn.bounding_box import bounding_box
from jimn.precision import is_almost
from jimn.iterators import all_two_elements


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

    def round_coordinates(self, milling_diameter):
        for p in self.points:
            p.round_coordinates(self, milling_diameter)

    def cut_sides(self, milling_diameter):
        elementary_segments = []
        segments = list(self.segments())
        preceding_segments = segments[-1:] + segments[:-1]
        current_segments = segments
        following_segments = segments[1:] + segments[:1]
        for p, c, f in zip(preceding_segments, current_segments, following_segments):
            elementary_segments.extend(c.cut(milling_diameter, p, f))
        return elementary_segments

    def create_vertices(self, milling_diameter):
        elementary_segments = self.cut_sides(milling_diameter)

        elementary_segments = reorder_elementary_segments_to_start_at_vertex(elementary_segments, milling_diameter)

        vertices = []
        intermediate_path = []
        final_vertex = vertex(elementary_segments[0].get_endpoint(0))
        previous_vertex = final_vertex
        for s in elementary_segments:
            p = s.get_endpoint(1)
            intermediate_path.append(s)
            if p.is_vertex:
                v = vertex(p)
                v.add_link(list(reversed(intermediate_path)))
                previous_vertex.add_link(intermediate_path)
                intermediate_path = []
                previous_vertex = v
                vertices.append(v)
        vertices[-1].add_link(final_vertex.get_link())

        return vertices

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
        display.write(" style=\"fill:{};stroke:{};stroke-width:1;opacity:0.4\" />".format(color, color))


def reorder_elementary_segments_to_start_at_vertex(elementary_segments, milling_diameter):
        # find first vertex
        for index, s in enumerate(elementary_segments):
            p = s.get_endpoint(0)
            if p.is_vertex:
                start = index
                break
        else:
            raise NoVertex

        return (elementary_segments[start:] + elementary_segments[:start])


class NoVertex(Exception):
    pass

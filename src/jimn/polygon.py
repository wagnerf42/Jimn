_squares_counter = 0


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

    def get_label(self):
        return self.label

    @classmethod
    def square(cls, start_x, start_y, side):
        """
        creates a square, horizontally aligned.
        used in many test scripts as a quick way to get polygons
        """
        global _squares_counter
        starting_point = Point([start_x, start_y])
        points = [
            Point([0.0, 0.0]),
            Point([side, 0.0]),
            Point([side, side]),
            Point([0.0, side]),
        ]
        points = [p + starting_point for p in points]
        square_polygon = cls(points, _squares_counter)
        _squares_counter += 1
        return square_polygon

    def remove_useless_points(self):
        """when 3 consecutive points are aligned the middle one is useless.
        we remove here all useless points in order to decrease cost of storage
        and computations of further operations.
        WARNING : this operation reverses orientation
        """
        self._remove_near_points()
        self._set_non_removable_start()
        p1 = self.points[0]
        p2 = self.points[1]
        kept_points = [p1]
        self.points.append(p1)  # go until start again
        # follow edge of polygon, looking for useless points
        for p3 in self.points[2:]:
            assert not p1.is_almost(p2)
            if p1.is_aligned_with(p2, p3):
                p2 = p3
            else:
                kept_points.append(p2)
                p1 = p2
                p2 = p3

        # TODO: remove reversed
        self.points = list(reversed(kept_points))

        if __debug__:
            if len(self.points) <= 2:
                raise invalid_polygon("not enough points after simplifying")

    def get_points(self):
        return self.points

    def segments(self):
        for p1, p2 in all_two_elements(self.points):
            yield Segment([p1, p2])

    def area(self):
        a = 0
        for p1, p2 in all_two_elements(self.points):
            (x1, y1), (x2, y2) = [p.get_coordinates() for p in (p1, p2)]
            a = a + x1*y2 - x2*y1
        return a/2

    def is_oriented_clockwise(self):
        """clockwise is defined respectively to svg displayed"""
        a = self.area()
        assert not is_almost(a, 0), "flat polygon"
        return a > 0

    def orient(self, clockwise=True):
        if self.is_oriented_clockwise() != clockwise:
            self.points.reverse()
        return self

    def normalize_starting_point(self):
        smallest_point = self.points[0]
        index = 0
        for k, p in enumerate(self.points):
            if p < smallest_point:
                smallest_point = p
                index = k
        self.points = self.points[index:] + self.points[:index]

    def translation_vector(self, p2, vector=None):
        """
        returns translation vector from self to obtain p2.
        'none' if impossible.
        you can optionnaly give desired translation vector.
        assumes the two polygons are normalized
        """
        for a1, a2 in zip(self.get_points(), p2.get_points()):
            if vector is None:
                vector = a2 - a1
            else:
                if not (a2 - a1).is_almost(vector):
                    return None
        return vector

    def __str__(self):
        strings = [str(p) for p in self.points]
        return "[" + str(self.label) + "/\npolygon([\n    " + \
            ",\n    ".join(strings) + "\n])\n]"

    def get_bounding_box(self):
        box = Bounding_Box.empty_box(2)
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

    def _remove_near_points(self):
        """
        keeps one of each almost identical points.
        """
        self.points = [
            p1 for p1, p2 in all_two_elements(self.points)
            if not p1.is_almost(p2)
        ]

    def _set_non_removable_start(self):
        """
        changes start point to one which cannot be removed when removing
        useless points later on.
        assumes no identical points in polygon.
        """
        for i, p in enumerate(self.points):
            p_prec = self.points[i-1]
            p_next = self.points[(i+1) % len(self.points)]
            if not p_prec.is_aligned_with(p, p_next):
                self.points = self.points[i:] + self.points[:i]
                return
        raise Exception("flat polygon")

from jimn.point import Point
from jimn.segment import Segment
from jimn.bounding_box import Bounding_Box
from jimn.utils.precision import is_almost
from jimn.utils.iterators import all_two_elements

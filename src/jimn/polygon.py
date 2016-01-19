"""
polygons.
"""
from jimn.point import Point
from jimn.segment import Segment
from jimn.bounding_box import Bounding_Box
from jimn.utils.precision import is_almost
from jimn.utils.iterators import all_two_elements


class Polygon:
    """
    a polygon is an ordered set of points.
    they can optionaly be labeled for better display.
    """
    squares_counter = 0

    def __init__(self, points, label=None):
        assert len(points) > 2
        self.points = points
        if label is None:
            self.label = id(self)
        else:
            self.label = label

    def get_label(self):
        """
        return polygon label.
        """
        return self.label

    @classmethod
    def square(cls, start_x, start_y, side):
        """
        create a square, horizontally aligned.
        used in many test scripts as a quick way to get polygons.
        """
        starting_point = Point([start_x, start_y])
        points = [
            Point([0.0, 0.0]),
            Point([side, 0.0]),
            Point([side, side]),
            Point([0.0, side]),
        ]
        points = [p + starting_point for p in points]
        square_polygon = cls(points, cls.squares_counter)
        cls.squares_counter += 1
        return square_polygon

    def remove_useless_points(self):
        """when 3 consecutive points are aligned the middle one is useless.
        we remove here all useless points in order to decrease cost of storage
        and computations of further operations.
        WARNING : this operation reverses orientation.
        """
        #  self._remove_near_points()  # TODO: useless for hashed polygons ?
        self._set_non_removable_start()
        last_kept_point = self.points[0]
        kept_points = [last_kept_point]
        middle_point = self.points[1]
        self.points.append(last_kept_point)  # go until start again
        # follow edge of polygon, looking for useless points
        for new_point in self.points[2:]:
            if last_kept_point.is_aligned_with(middle_point, new_point):
                middle_point = new_point  # skip middle point
            else:
                kept_points.append(middle_point)
                last_kept_point = middle_point
                middle_point = new_point

        self.points = kept_points

        assert len(self.points) > 2

    def segments(self):
        """
        iterate through all segments.
        """
        for points in all_two_elements(self.points):
            yield Segment(points)

    def area(self):
        """
        return polygon area. can be positive or negative, depending on
        orientation.
        """
        area = 0
        for points in all_two_elements(self.points):
            area += points[0].cross_product(points[1])
        return area/2

    def is_oriented_clockwise(self):
        """
        clockwise being defined respectively to svg displayed, return
        true if polygon is oriented clockwise."""
        area = self.area()
        assert not is_almost(area, 0), "flat polygon"
        return area > 0

    def orient(self, clockwise=True):
        """
        orient polygon with given orientation
        """
        if self.is_oriented_clockwise() != clockwise:
            self.points.reverse()
        return self

    def normalize_starting_point(self):
        """
        set smallest point as starting point.
        this allows easy identification of translated polygons.
        """
        index = min(range(len(self.points)), key=lambda i: self.points[i])
        self.points = self.points[index:] + self.points[:index]

    def translation_vector(self, other, vector=None):
        """
        return translation vector from self to obtain p2.
        'none' if impossible.
        you can optionnaly give desired translation vector.
        assumes the two polygons are normalized
        """
        for point_on_self, point_on_other in zip(self.points, other.points):
            if vector is None:
                vector = point_on_self - point_on_other
            else:
                if not (point_on_self - point_on_other).is_almost(vector):
                    return None
        return vector

    def __str__(self):
        strings = [str(p) for p in self.points]
        return "[" + str(self.label) + "/\npolygon([\n    " + \
            ",\n    ".join(strings) + "\n])\n]"

    def get_bounding_box(self):
        """
        min bounding box containing polygon.
        """
        box = Bounding_Box.empty_box(2)
        for point in self.points:
            box.add_point(point)
        return box

    def save_svg_content(self, display, color):
        """
        svg for tycat.
        """
        svg_coordinates = [
            "{},{}".format(
                *display.convert_coordinates(p.get_coordinates())
            )
            for p in self.points
        ]
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
        for i, point in enumerate(self.points):
            preceding_point = self.points[i-1]
            next_point = self.points[(i+1) % len(self.points)]
            if not preceding_point.is_aligned_with(point, next_point):
                self.points = self.points[i:] + self.points[:i]
                return
        raise Exception("flat polygon")

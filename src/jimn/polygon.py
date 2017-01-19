"""
polygons.
"""
from jimn.point import Point
from jimn.segment import Segment
from jimn.bounding_box import BoundingBox
from jimn.utils.precision import is_almost
from jimn.utils.iterators import all_two_elements, all_three_elements
from jimn.utils.tour import tour


class Polygon:
    """
    a polygon is an ordered set of points.

    for example:

    - create a triangle:

    triangle = Polygon([Point([0, 0]), Point([1, 1]), Point([2, 0])])

    - loop on all segments in a polygon:

    for segment in polygon.segments():
        ....

    """

    def __init__(self, points):
        assert len(points) > 2
        self.points = points

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
        square_polygon = cls(points)
        return square_polygon

    def remove_useless_points(self):
        """
        when 3 consecutive points are aligned the middle one is useless.
        we remove here all useless points in order to decrease cost of storage
        and computations of further operations.
        """
        # first remove all tiny artifacts
        removed_points = set()
        for points in all_three_elements(self.points):
            if abs(Polygon(points).area()) < 0.000001:
                removed_points.add(points[1])

        self.points = [p for p in self.points if p not in removed_points]

        # now remove aligned points
        removed_points = set()
        for points in all_three_elements(self.points):
            if points[0].is_aligned_with(points[1], points[2]):
                removed_points.add(points[1])

        new_points = [p for p in self.points if p not in removed_points]
        assert len(new_points) > 2
        return Polygon(new_points)

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
        return sum([p1.cross_product(p2)
                    for p1, p2 in all_two_elements(self.points)]) / 2

    def is_oriented_clockwise(self):
        """
        clockwise being defined respectively to svg displayed, return
        true if polygon is oriented clockwise.
        """
        area = self.area()
        assert not is_almost(area, 0), "flat or crossing polygon"
        return area > 0

    def orient(self, clockwise=True):
        """
        orient polygon with given orientation
        """
        if self.is_oriented_clockwise() != clockwise:
            return Polygon(self.points[::-1])
        else:
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
        if len(self.points) != len(other.points):
            return None
        for point_on_self, point_on_other in zip(self.points, other.points):
            if vector is None:
                vector = point_on_other - point_on_self
            else:
                if not (point_on_other - point_on_self).is_almost(vector):
                    return None
        return vector

    def get_bounding_box(self):
        """
        min bounding box containing polygon.
        """
        box = BoundingBox.empty_box(2)
        for point in self.points:
            box.add_point(point)
        return box

    def svg_content(self):
        """
        svg for tycat.
        """
        svg_coordinates = [
            "{},{}".format(*p.coordinates)
            for p in self.points
        ]
        svg_formatted = " ".join(svg_coordinates)
        return '<polygon points="{}" opacity="0.4"/>'.format(svg_formatted)

    def __str__(self):
        points = ",\n".join(str(p) for p in self.points)
        return "Polygon([" + points + "])\n"

    def __eq__(self, other):
        """
        return true if self is almost other.
        requires both polygons to be normalized.
        """
        if len(self.points) != len(other.points):
            return False

        for point1, point2 in zip(self.points, other.points):
            if not point1.is_almost(point2):
                return False

        return True


def _tour():
    description = "we provide a 'Polygon' class."
    example = """
from jimn.point import Point
from jimn.polygon import Polygon
from jimn.displayable import tycat
points = list()
for x in range(4):
    points.append(Point([x, x*x]))
polygon = Polygon(points)
tycat(polygon)
tycat(*list(polygon.segments()))
    """
    tour("jimn.polygon", description, example)

if __name__ == "__main__":
    _tour()

from jimn.segment import segment
from jimn.polygon import polygon
from jimn.displayable import tycat
from jimn.debug import is_module_debugged
from jimn.point import point
from jimn.precision import is_almost
from jimn.iterators import all_two_elements, all_three_elements
from math import pi, sin, cos

"""requires polygon to be oriented counter clockwise to carve the inside
and clockwise to carve the outside"""


class offseter:
    def __init__(self, radius, polygon):
        self.polygon = polygon
        self.radius = radius

    def raw_offset(self):
        old_points = self.polygon.get_points()
        new_points = []
        # move each outer edge point inside the polygon
        for p1, p2, p3 in all_three_elements(old_points):
            p = self.compute_inner_p2_point(p1, p2, p3)
            new_points.append(p)

        if __debug__:
            if is_module_debugged(__name__):
                tycat(self.polygon, polygon(new_points))

        return [
            segment([p1, p2]) for p1, p2 in all_two_elements(new_points)
        ]

    """use radius to find nearest place towards p2 to place a circle"""
    def compute_inner_p2_point(self, p1, p2, p3):

        base_angle = p2.angle_with(p3)
        angle = p2.angle_with(p1) - base_angle
        assert not is_almost(angle, 0), "flat segment"

        # compute distance
        if angle < 0:
            angle += 2*pi

        if is_almost(angle, pi) or angle > pi:
            d = self.radius
        else:
            d = self.radius/sin(angle/2)

        # compute point
        a = base_angle + angle / 2
        # use opposite of angle because angles are computed reversed
        # (see jimn.point : angle_with)
        p = point([cos(-a)*d, sin(-a)*d]) + p2

        if __debug__:
            if is_module_debugged(__name__):
                print("angle :", angle)
                print("distance :", d)
                tycat(self.polygon, p1, p2, p3, p)

        return p


def raw_offset(radius, polygon_to_offset):
    o = offseter(radius, polygon_to_offset)
    segments = o.raw_offset()
    if len(segments) < 2:
        return []
    else:
        return segments


def offset_holed_polygon(radius, *polygons):
    segments = []
    for p in polygons:
        segments.extend(raw_offset(radius, p))
    tycat(segments)

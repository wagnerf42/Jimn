from jimn.bounding_box import bounding_box
from jimn.point import point
from jimn.segment import segment
from jimn.precision import is_almost
from math import sqrt, pi


class arc:
    def __init__(self, radius, points):
        self.radius = radius
        self.points = points
        self.center = self.compute_center()

    def compute_center(self):
        middle = segment(self.points).middle_point()
        return self.points[1].rotate_around(middle, -pi/2)

    def get_bounding_box(self):
        # TODO: this is not good
        box = bounding_box.empty_box(2)
        for p in self.points:
            box.add_point(p)
        return box

    def contains(self, p):
        """return true if point p is inside arc"""
        if p.is_almost(self.points[0]) or p.is_almost(self.points[1]):
            return True
        if not is_almost(self.center.distance_to(p), self.radius):
            return False
        diff = self.points[1] - self.points[0]
        product = diff.scalar_product(p)
        assert not is_almost(product, 0), "already tested at entry of method"
        return (product > 0)

    def intersection_with_arc(self, other, rounder):
        points = circles_intersections(self.center, other.center,
                                       self.radius, other.radius, rounder)
        for p in points:
            if self.contains(p):
                return p

    def save_svg_content(self, display, color):
        x1, y1, x2, y2 = [
            c for p in self.points
            for c in display.convert_coordinates(p.get_coordinates())
        ]
        r = display.stretch * self.radius
        self.center.save_svg_content(display, color)
        display.write('<path d="M{},{} A{},{} 0 0,1 {},{}" \
                      fill="none" stroke="{}" \
                      opacity="0.5" stroke-width="3"\
                      />'.format(x1, y1, r, r, x2, y2, color))


def circles_intersections(c1, c2, r1, r2, rounder):
    d = c1.distance_to(c2)
    if is_almost(d, 0):
        return []  # common center
    x1, y1, x2, y2 = [c for p in (c1, c2) for c in p.get_coordinates()]
    if is_almost(r1, r2):
        l = d/2
    else:
        l = (r1 * r1 - r2 * r2 + d * d) / (2 * d)
    if r1 < l:
        return []  # too far away

    h = sqrt(r1 * r1 - l * l)
    points = [
        point([
            l/d * (x2 - x1) + h/d * (y2 - y1) + x1,
            l/d * (y2 - y1) - h/d * (x2 - x1) + y1
        ]),
        point([
            l/d * (x2 - x1) - h/d * (y2 - y1) + x1,
            l/d * (y2 - y1) + h/d * (x2 - x1) + y1
        ])
    ]
    if is_almost(r1, l):
        # only one intersection
        return [rounder.hash_point(points[0])]
    else:
        return [rounder.hash_point(p) for p in points]

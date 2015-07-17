from jimn.elementary_path import elementary_path
from jimn.bounding_box import bounding_box
from jimn.point import point
from jimn.precision import is_almost
from jimn.displayable import tycat
from jimn.math import solve_quadratic_equation
from math import sqrt, pi


class arc(elementary_path):
    def __init__(self, radius, points):
        super().__init__(points)
        self.radius = radius
        assert self.radius > 0, "0 or negative radius"
        self.center = self.compute_center()

    def compute_center(self):
        middle = self.middle_point()
        return self.endpoints[1].rotate_around(middle, -pi/2)

    def get_bounding_box(self):
        box = bounding_box.empty_box(2)
        box.add_point(self.center + point([self.radius, self.radius]))
        box.add_point(self.center - point([self.radius, self.radius]))
        return box

    def contains(self, p):
        """return true if point p is inside arc"""
        if p.is_almost(self.endpoints[0]) or p.is_almost(self.endpoints[1]):
            return True
        if not is_almost(self.center.distance_to(p), self.radius):
            return False
        diff = self.endpoints[1] - self.endpoints[0]
        diff_p = p - self.endpoints[0]
        product = diff.scalar_product(diff_p)
        assert not is_almost(product, 0), "already tested at entry of method"
        return (product > 0)

    def intersections_with_arc(self, other, rounder):
        points = circles_intersections(self.center, other.center,
                                       self.radius, other.radius, rounder)
        intersections = []
        for p in points:
            if self.contains(p) and other.contains(p):
                intersections.append(p)
        return intersections

    def intersections_with_segment(self, intersecting_segment, rounder):
        points = intersecting_segment.get_endpoints()
        # take first point as origin
        d = points[1] - points[0]
        c = self.center - points[0]
        xd, yd = d.get_coordinates()
        xc, yc = c.get_coordinates()
        # segment points are at alpha * d
        # distance(alpha * d, center) = r

        # (xc-alpha*xd)**2 + (yc-alpha*yd)**2 - r**2 = 0

        # xc**2 + alpha**2*xd**2 -2*alpha*xc*xd
        # yc**2 + alpha**2*yd**2 -2*alpha*yc*yd
        # - r**2 = 0
        a = xd**2 + yd**2
        b = -2*(xc*xd + yc*yd)
        c = xc**2 + yc**2 - self.radius**2

        solutions = solve_quadratic_equation(a, b, c)
        intersections = []
        for s in solutions:
            if is_almost(s, 0) or s > 0:
                if is_almost(s, 1) or s < 1:
                    intersections.append(rounder.hash_point(points[0]+d*s))

        return intersections

    def save_svg_content(self, display, color):
        x1, y1, x2, y2 = [
            c for p in self.endpoints
            for c in display.convert_coordinates(p.get_coordinates())
        ]
        r = display.stretch() * self.radius
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

    if is_almost(r1, l):
        # only one intersection
        i = point([
            l/d * (x2 - x1) + x1,
            l/d * (y2 - y1) + y1
        ])
        return [rounder.hash_point(i)]
    else:
        if abs(r1) < abs(l):
            return []
        else:
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
            return [rounder.hash_point(p) for p in points]

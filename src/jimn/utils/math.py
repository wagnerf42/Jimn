from math import sqrt
from jimn.utils.precision import is_almost
from jimn.point import point


def solve_quadratic_equation(a, b, c):
    """ solves a*x*x + b*y +c = 0
    careful : we do some rounding here:
    when delta is close from 0 we round it towards 0
    do not use if you do not understand what it does"""
    delta = b * b - 4 * a * c
    if is_almost(delta, 0):
        return [-b/(2*a)]
    else:
        if delta < 0:
            return []
        else:
            return [(-b-sqrt(delta))/(2*a), (-b+sqrt(delta))/(2*a)]


def circles_intersections(c1, c2, r1, r2, rounder):
    d = c1.distance_to(c2)
    if is_almost(d, 0):
        return []  # common center
    x1, y1, x2, y2 = [c for p in (c1, c2) for c in p.get_coordinates()]
    if is_almost(r1, r2):
        l = d/2
    else:
        l = (r1 * r1 - r2 * r2) / (2 * d) + d/2

    if is_almost(r1, l):
        # only one intersection
        i = point([
            l/d * (x2 - x1) + x1,
            l/d * (y2 - y1) + y1
        ])
        return [rounder.hash_point(i)]
    else:
        if r1 < l:
            return []  # too far away

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


def line_circle_intersections(points, center, radius, rounder):
    # take first point as origin
    d = points[1] - points[0]
    c = center - points[0]
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
    c = xc**2 + yc**2 - radius**2

    solutions = solve_quadratic_equation(a, b, c)
    intersections = []
    for s in solutions:
        intersection = points[0] + d * s
        intersections.append(rounder.hash_point(intersection))
    return intersections

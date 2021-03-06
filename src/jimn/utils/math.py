"""
all math related helpers.
"""
from math import sqrt, floor, ceil
from jimn.utils.precision import is_almost
from jimn.point import Point


# pylint: disable=invalid-name
def solve_quadratic_equation(a, b, c):
    """ solves a*x*x + b*y +c = 0
    careful : we do some rounding here:
    when delta is close from 0 we round it towards 0
    do not use if you do not understand what it does"""
    delta = b * b - 4 * a * c
    if is_almost(sqrt(abs(delta)), 0):
        if is_almost(a, 0):
            return []
        return [-b/(2*a)]
    else:
        if delta < 0:
            return []
        else:
            return [(-b-sqrt(delta))/(2*a), (-b+sqrt(delta))/(2*a)]


def circles_intersections(c1, c2, r1, r2):
    """
    intersect two circles with given centers and radiuses.
    return points array.
    """
    d = c1.distance_to(c2)
    if is_almost(d, 0):
        return []  # common center
    x1, y1, x2, y2 = [c for p in (c1, c2) for c in p.coordinates]
    if is_almost(r1, r2):
        l = d/2
    else:
        l = (r1 * r1 - r2 * r2) / (2 * d) + d/2

    if is_almost(r1, l):
        # only one intersection
        i = Point([
            l/d * (x2 - x1) + x1,
            l/d * (y2 - y1) + y1
        ])
        return [i]
    else:
        if r1 < l:
            return []  # too far away

        if abs(r1) < abs(l):
            return []
        else:
            h = sqrt(r1 * r1 - l * l)
            points = [
                Point([
                    l/d * (x2 - x1) + h/d * (y2 - y1) + x1,
                    l/d * (y2 - y1) - h/d * (x2 - x1) + y1
                ]),
                Point([
                    l/d * (x2 - x1) - h/d * (y2 - y1) + x1,
                    l/d * (y2 - y1) + h/d * (x2 - x1) + y1
                ])
            ]
            return points


def vline_circle_intersections(x, center, radius):
    """
    intersection of circle with vertical line at given x.
    """
    distance = abs(x - center.get_x())
    if is_almost(radius, distance):
        return [Point([x, center.get_y()])]
    squared_distance = distance * distance
    squared_radius = radius * radius
    if squared_distance > squared_radius:
        return []
    y = sqrt(squared_radius - squared_distance)
    return [Point([x, center.get_y() + y]), Point([x, center.get_y() - y])]


def line_circle_intersections(points, center, radius):
    """
    intersection of line going through points with given circle.
    """
    # take first point as origin
    d = points[1] - points[0]
    c = center - points[0]
    xd, yd = d.coordinates
    xc, yc = c.coordinates
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
    return [points[0] + d * s for s in solutions]


def milling_heights(y1, y2, milling_diameter, inclusive=False):
    """
    iterate in order on all y between y1 and y2 on milling heights
    if inclusive possibly includes y1 and y2.
    """
    if y1 < y2:
        index = ceil(y1/milling_diameter)
        step = 1
    else:
        index = floor(y1/milling_diameter)
        step = -1

    y = index * milling_diameter
    if not inclusive:
        if y == y1:
            index += step
            y = index * milling_diameter

    while step*y < step*y2:
        yield y
        index += step
        y = index * milling_diameter

    if inclusive:
        if y == y2:
            yield y


def is_slice_height(y, milling_diameter):
    """
    is given coordinate on a slice height ?
    """
    d = milling_diameter
    return is_almost(y/d, round(y/d))


def compute_arc_centers(radius, points):
    """
    return list of possible centers for an arc of given radius going
    through given points.
    """
    # take points[0] as origin
    point2 = points[1] - points[0]
    # find bisector
    middle = point2/2
    bisector_point = middle + point2.perpendicular_vector()
    # intersect with circle at origin
    intersections = line_circle_intersections(
        [middle, bisector_point],
        Point([0, 0]),
        radius
    )
    assert len(intersections) == 2, "invalid arc"
    centers = [points[0] + i for i in intersections]
    return centers

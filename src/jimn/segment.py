# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.point import point
from jimn.coordinates_hash import coordinates_hash
from jimn.precision import segment_limit, check_precision, is_almost
from jimn.bounding_box import bounding_box
from math import pi, cos, sin, ceil, floor

rounding_hash = coordinates_hash(3)


class segment:
    """A segment is defined as a set of two points"""

    def __init__(self, points):
        self.endpoints = points
        assert self.endpoints[0].dimension() == self.endpoints[1].dimension()
        if __debug__:
            if(self.squared_length() <= segment_limit):
                # print("very small segment {}".format(str(self)), file=sys.stderr)
                raise Exception("very small segment")

    def reverse(self):
        return segment(list(reversed(self.endpoints)))

    @classmethod
    def horizontal_segment(cls, y):
            coordinates = ([0.0, y], [1.0, y])
            return cls([point(c) for c in coordinates])

    def __str__(self):
        return "[{}]".format(';'.join(map(lambda p: str(p), self.endpoints)))

    def middle_point(self):
        p1, p2 = self.endpoints
        return p1 + (p2-p1) / 2

    def has_extremity(self, intermediate_point):
        for p in self.endpoints:
            if p == intermediate_point:
                return True
            else:
                assert not p.is_almost(intermediate_point), "precision pb"
        return False

    def split_at(self, points):
        points.extend(self.endpoints)
        sorted_points = sorted(points)
        segments = [
            segment([p1, p2]) for p1, p2 in zip(
                sorted_points[:-1],
                sorted_points[1:]
            )
        ]
        return segments

    def squared_length(self):
        return self.endpoints[0].squared_distance_to(self.endpoints[1])

    def get_bounding_box(self):
        boxes = [bounding_box(p.get_coordinates(), p.get_coordinates()) for p in self.endpoints]
        boxes[0].update(boxes[1])
        return boxes[0]

    def save_svg_content(self, display, color):
        svg_coordinates = [
            c for point in self.endpoints
            for c in display.convert_coordinates(point.get_coordinates())
        ]
        display.write("<line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\"".format(*svg_coordinates))
        display.write(" stroke-width=\"3\" stroke=\"{}\" opacity=\"0.5\"/>\n".format(color))

    def smallest_point(self):
        return min(*self.endpoints)

    def horizontal_plane_intersection(self, h):
        assert self.dimension() == 3
        p1, p2 = self.endpoints
        x1, y1, z1 = p1.get_coordinates()
        x2, y2, z2 = p2.get_coordinates()

        if __debug__:
            check_precision(z1, z2, 'horizontal_plane_intersection')
        z = h
        x = x1 + (z - z1)/(z2 - z1)*(x2 - x1)
        y = y1 + (z - z1)/(z2 - z1)*(y2 - y1)

        return rounding_hash.hash_point(point([x, y, z]))

    def dimension(self):
        return self.endpoints[0].dimension()

    def is_below(self, p):
        [a, b] = self.get_endpoints()
        [x0, y0] = p.get_coordinates()
        if a.get_x() == b.get_x():
            assert(a.get_y() <= b.get_y())
            return y0 >= a.get_y()
        else:
            # TODO: check precision of division
            alpha = (b.get_y() - a.get_y()) / (b.get_x() - a.get_x())
            beta = a.get_y() - alpha * a.get_x()
            return y0 >= alpha * x0 + beta

    def is_vertical_3d(self):
        xa, xb = [p.get_x() for p in self.endpoints]
        ya, yb = [p.get_y() for p in self.endpoints]
        if xa == xb and ya == yb:
            return True
        assert not(is_almost(xa, xb) and is_almost(ya, yb)),  "near vertical 3d"
        return False

    # for 2d points
    def is_vertical(self):
        xa, xb = [p.get_x() for p in self.endpoints]
        if xa == xb:
            return True
        else:
            if is_almost(xa, xb):
                raise RuntimeError("almost vertical segment")
            return False

    # return unique id of line on which is segment
    def line_hash(self, rounder):
        assert self.dimension() == 2, 'only works on 2d points segment'
        (x1, y1), (x2, y2) = [p.get_coordinates() for p in self.endpoints]
        if x1 == x2:
            key = rounder.hash_coordinate(0, x1)
            return ':{}'.format(key)
        else:
            if __debug__:
                check_precision(x1, x2, 'line_hash')
            a = (y2-y1)/(x2-x1)
            b = y1 - a * x1
            key_a = rounder.hash_coordinate(1, a)
            key_b = rounder.hash_coordinate(1, b)
            return '{}:{}'.format(key_a, key_b)

    def projection2d(self):
        p1, p2 = self.endpoints
        return segment([p1.projection2d(), p2.projection2d()])

    def get_endpoint(self, index):
        return self.endpoints[index]

    def set_endpoint(self, index, new_point):
        self.endpoints[index] = new_point

    def get_endpoints(self):
        return self.endpoints

    def sort_endpoints(self):
        return segment(sorted(self.endpoints))

    def is_sorted(self):
        sorted_self = self.sort_endpoints()
        return sorted_self.endpoints == self.endpoints

    def angle(self):
        return self.endpoints[0].angle_with(self.endpoints[1])

    def intersection_with(self, other, rounder):
        """
        intersect two segments.
        only return point if included on the two segments.
        """
        assert self.dimension() == 2, "non 2d intersections"
        assert other.dimension() == 2, "non 2d intersections"
        # prepare rounder
        for s in (self, other):
            for p in s.get_endpoints():
                rounder.hash_point(p)
        # compute point
        i = self.line_intersection_with(other)
        if i is None:
            return  # parallel lines
        # check validity
        i = rounder.hash_point(i)
        for s in (self, other):
            if not s.get_bounding_box().contains_point(i):
                return
        return i

    def line_intersection_with(self, other):
        """returns point intersecting with the two lines passing through the segments.
        None if lines are almost parallel
        """
        x1, y1, x2, y2, x3, y3, x4, y4 = [
            c for s in (self, other)
            for p in s.get_endpoints()
            for c in p.get_coordinates()
        ]
        denominator = (x1-x2) * (y4-y3) + (x3-x4) * (y1-y2)
        if is_almost(denominator, 0):
            return
        x = x1*(x3*y4-x4*y3)+x2*(x4*y3-x3*y4)+(x3-x4)*(y1*x2-x1*y2)
        x /= denominator
        y = y2*(x1*(y4-y3)-x3*y4+x4*y3) + y1*(x3*y4+x2*(y3-y4)-x4*y3)
        y /= denominator
        return point([x, y])

    def __eq__(a, b):
        return a.endpoints == b.endpoints

    def __hash__(self):
        return hash(tuple(self.endpoints))

    def parallel_segment(self, distance):
        a = self.endpoints[0].angle_with(self.endpoints[1])
        a += pi/2
        displacement = point([
            distance * cos(-a),
            distance * sin(-a)
        ])
        return segment([
            p + displacement for p in self.endpoints
        ])

    def intersection_with_slices(self, milling_diameter):
        d = milling_diameter
        ya, yb = [p.get_y() for p in self.get_endpoints()]

        if ya > yb:
            # compute height of slice center strictly above a
            na = ceil(ya/d) - 1
            # compute height of slice center above b
            nb = floor(yb/d)
        else:
            # compute height of slice center strictly below a
            na = floor(ya/d) + 1
            # compute height of slice center below b
            nb = ceil(yb/d)

        steps = [-1, 1]
        step = steps[ya <= yb]
        slice_numbers = range(na, nb, step)
        slice_heights = [n * d for n in slice_numbers]
        slice_segments = [segment.horizontal_segment(h) for h in slice_heights]
        intersection_points = [self.line_intersection_with(s) for s in slice_segments]
        for h, p in zip(slice_heights, intersection_points):
            p.set_y(h)

        return intersection_points

    def cut(self, milling_diameter, previous_segment, next_segment):
        a, b = self.endpoints
        a.mark(vertex=vertex_corner(previous_segment, self))
        b.mark(vertex=vertex_corner(self, next_segment))
        ya, yb = [p.get_y() for p in self.endpoints]

        if ya == yb:
            return [self]

        intersection_points = self.intersection_with_slices(milling_diameter)
        for p in intersection_points:
            p.mark(vertex=True)
        # elementary_segments = self.split_at(intersection_points)
        # TODO : put in separate function
        points = [a] + intersection_points + [b]
        segments = [
            segment([p1, p2])
            for p1, p2 in zip(points[:-1], points[1:])
        ]

        return segments

    def dy(self):
        ya, yb = [p.get_y() for p in self.get_endpoints()]
        return yb - ya


def vertex_corner(s1, s2):
    if s1.dy() == 0 or s2.dy() == 0:
        return False
    d1, d2 = [s.dy() > 0 for s in [s1, s2]]
    return d1 == d2

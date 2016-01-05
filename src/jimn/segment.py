from jimn.elementary_path import elementary_path


class segment(elementary_path):
    def __init__(self, points):
        super().__init__(points)

    @classmethod
    def horizontal_segment(cls, xmin, xmax, y):
            coordinates = ([xmin, y], [xmax, y])
            return cls([point(c) for c in coordinates])

    def split_at_milling_points(self, milling_diameter):
        """
        returns array of segments obtained when stopping at each milling height
        """
        y1, y2 = [p.get_y() for p in self.endpoints]
        points = [self.endpoints[0]]
        for y in milling_heights(y1, y2, milling_diameter):
            points.append(self.horizontal_intersection_at(y))
        points.append(self.endpoints[1])

        try:
            chunks = [
                segment([points[i], points[i+1]])
                for i in range(len(points)-1)
            ]
        except:
            print("failed splitting", self, "for diameter", milling_diameter)
            raise
        return chunks

    def horizontal_intersection_at(self, y):
        """
        returns point on self at given y.
        precondition : y is valid height in segment
        """
        (x1, y1), (x2, y2) = [p.get_coordinates() for p in self.endpoints]
        if is_almost(x1, x2):
            return point([x1, y])
        else:
            a = (y1 - y2)/(x1 - x2)
            x = (y - y1) / a + x1
            return point([x, y])

    def reverse(self):
        """invert endpoints"""
        return segment(list(reversed(self.endpoints)))

    def __str__(self):
        return "segment([" + str(self.endpoints[0]) + ", " + \
            str(self.endpoints[1]) + "])"

    def get_bounding_box(self):
        boxes = [
            bounding_box(p.get_coordinates(), p.get_coordinates())
            for p in self.endpoints
        ]
        boxes[0].update(boxes[1])
        return boxes[0]

    def save_svg_content(self, display, color):
        svg_coordinates = [
            c for point in self.endpoints
            for c in display.convert_coordinates(point.get_coordinates())
        ]
        stroke_width = display.stroke_width()
        display.write("<line x1=\"{}\" y1=\"{}\"\
                      x2=\"{}\" y2=\"{}\"".format(*svg_coordinates))
        display.write(" stroke-width=\"{}\" stroke=\"{}\"\
                      opacity=\"0.5\"/>\n".format(stroke_width, color))
        # have a small arrow
        center = self.endpoints[0]*0.5 + self.endpoints[1]*0.5
        before = self.endpoints[0]*0.6 + self.endpoints[1]*0.4
        top = before.rotate_around(self.endpoints[0], pi/20)
        bottom = before.rotate_around(self.endpoints[0], -pi/20)
        top_coordinates = [
            c for point in [center, top]
            for c in display.convert_coordinates(point.get_coordinates())
        ]
        bottom_coordinates = [
            c for point in [center, bottom]
            for c in display.convert_coordinates(point.get_coordinates())
        ]
        display.write("<line x1=\"{}\" y1=\"{}\"\
                      x2=\"{}\" y2=\"{}\"".format(*top_coordinates))
        display.write(" stroke-width=\"{}\" stroke=\"{}\"\
                      opacity=\"0.5\"/>\n".format(stroke_width, color))
        display.write("<line x1=\"{}\" y1=\"{}\"\
                      x2=\"{}\" y2=\"{}\"".format(*bottom_coordinates))
        display.write(" stroke-width=\"{}\" stroke=\"{}\"\
                      opacity=\"0.5\"/>\n".format(stroke_width, color))

    def horizontal_plane_intersection(self, h):
        """
        PREREQUISITE: 3d segment.
        cut it with plane at given height.
        requires h between hmin and hmax of segment
        """
        p1, p2 = self.endpoints
        x1, y1, z1 = p1.get_coordinates()
        x2, y2, z2 = p2.get_coordinates()

        if __debug__:
            check_precision(z1, z2, 'horizontal_plane_intersection')
        z = h
        x = x1 + (z - z1)/(z2 - z1)*(x2 - x1)
        y = y1 + (z - z1)/(z2 - z1)*(y2 - y1)

        return rounder2d.hash_point(point([x, y]))

    def is_vertical_3d(self):
        xa, xb = [p.get_x() for p in self.endpoints]
        ya, yb = [p.get_y() for p in self.endpoints]
        if xa == xb and ya == yb:
            return True
        assert not(is_almost(xa, xb) and is_almost(ya, yb)),  "near vertical 3d"
        return False

    # return unique id of line on which is segment
    def line_hash(self, rounder):
        assert self.dimension() == 2, 'only works on 2d points segment'
        (x1, y1), (x2, y2) = [p.get_coordinates() for p in self.endpoints]
        if is_almost(x1, x2):
            key = rounder.hash_coordinate(0, x1)
            return ':{}'.format(key)
        else:
            a = (y2-y1)/(x2-x1)
            b = y1 - a * x1
            key_a = rounder.hash_coordinate(1, a)
            key_b = rounder.hash_coordinate(1, b)
            return '{}:{}'.format(key_a, key_b)

    def projection2d(self):
        p1, p2 = self.endpoints
        return segment([p1.projection2d(), p2.projection2d()])

    def point_projection(self, p):
        """
        project p on line going through self
        """
        s = self.endpoints[1] - self.endpoints[0]
        v = p - self.endpoints[0]
        return self.endpoints[0] + s * (v.scalar_product(s)/s.scalar_product(s))

    def angle(self):
        return self.endpoints[0].angle_with(self.endpoints[1])

    def intersection_with_segment(self, other):
        """
        intersect two 2d segments.
        only return point if included on the two segments.
        """
        # compute point
        i = self.line_intersection_with(other)
        if i is None:
            return  # parallel lines

        # check validity
        for s in (self, other):
            if not s.get_bounding_box().almost_contains_point(i):
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

    def parallel_segment(self, distance, side=1):
        a = self.endpoints[0].angle_with(self.endpoints[1])
        a += side*pi/2
        displacement = point([
            distance * cos(-a),
            distance * sin(-a)
        ])
        return segment([p + displacement for p in self.endpoints])

    def contains(self, possible_point):
        distance = sum([possible_point.distance_to(p) for p in self.endpoints])
        return is_almost(distance,
                         self.endpoints[0].distance_to(self.endpoints[1]))

    def vertical_intersection_at(self, x):
        x1, y1 = self.endpoints[0].get_coordinates()
        x2, y2 = self.endpoints[1].get_coordinates()
        if x1 == x2:
            if not is_almost(x, x1):
                return None
            # when vertical, we return coordinate of lowest point
            return self.lowest_endpoint().get_y()
        if __debug__:
            check_precision(x1, x2, 'vertical_intersection_at')
        a = (y2-y1)/(x2-x1)
        y = y1 + a*(x-x1)
        return y

    def comparison(a, b):
        """
        returns if a < b.
        order has no real meaning. it is just an arbitrary order.
        precondition: both are segments
        """
        if a.endpoints[0].is_almost(b.endpoints[0]):
            if a.endpoints[1].is_almost(b.endpoints[1]):
                return
            else:
                return a.endpoints[1] < b.endpoints[1]
        else:
            return a.endpoints[0] < b.endpoints[0]

    def translate(self, translation):
        """
        translates segment by a given translation vector
        """
        return segment([p+translation for p in self.endpoints])

    def remove_overlap_with(self, other):
        """
        if self and other overlap return array of arrays containing non common
        parts (for self and for other). if no overlap returns None
        """
        if id(self) == id(other):
            return  # abort if with self
        if self.line_hash(rounder_lines) != other.line_hash(rounder_lines):
            return  # abort if segments are not aligned

        sides = (self, other)
        events = defaultdict(list)
        for side_number, side in enumerate(sides):
            for j, p in enumerate(side.endpoints):
                events[p].append((side_number, 2*j-1))

        inside = [0, 0]
        results = [[], []]
        entered = [None, None]
        overlap = False
        for p in sorted(list(events.keys())):
            old_inside = list(inside)
            for e in events[p]:
                inside[e[0]] += e[1]

            if abs(sum(old_inside)) == 1:
                # we were on only 1 path
                for side, count in enumerate(old_inside):
                    if abs(count) == 1:
                        # we are leaving kept part
                        if count == 1:
                            results[side].append(segment([p, entered[side]]))
                        else:
                            results[side].append(segment([entered[side], p]))

            if abs(inside[0]) == 1 and abs(inside[1]) == 1:
                overlap = True

            for side, count in enumerate(inside):
                if abs(count) == 1:
                    entered[side] = p

        if overlap:
            return results

from jimn.bounding_box import bounding_box
from jimn.point import point
from jimn.utils.coordinates_hash import rounder2d, rounder_lines
from jimn.utils.precision import check_precision, is_almost
from jimn.utils.math import milling_heights
from math import pi, cos, sin
from collections import defaultdict

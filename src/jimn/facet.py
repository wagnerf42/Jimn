# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.point import point
from jimn.segment import segment
import struct


class facet:
    def __init__(self, *points):
        self.points = [p for p in points]

    def __str__(self):
        return "[{}]".format(';'.join(map(lambda p: str(p), self.points)))

    def segments(self):
        p1, p2, p3 = self.points
        return [segment(p1, p2), segment(p1, p3), segment(p2, p3)]

    def add_point(self, p):
        self.points.append(p)

    def is_below(self, h):
        for p in self.points:
            if p.get_z() > h:
                return False
        return True

    def is_above(self, h):
        for p in self.points:
            if p.get_z() < h:
                return False
        return True

    def separate(self, h):
        inf_equal = []
        sup = []
        for p in self.points:
            if p.get_z() <= h:
                inf_equal.append(p)  # TODO: inf_equal or sup_equal ?
            else:
                sup.append(p)
        if len(inf_equal) == 2:
            return inf_equal, sup[0]
        else:
            return sup, inf_equal[0]

    def intersect(self, h):
        if self.is_above(h):
            return self.segments()

        if self.is_below(h):
            return []

        # separate points above and below, alone or not
        together_points, isolated_point = self.separate(h)

        traversing_segments = [segment(p, isolated_point) for p in together_points]
        intersections = [s.intersect(h) for s in traversing_segments]

        above_segments = [segment(*intersections)]
        z1 = together_points[0].get_z()
        if z1 <= h:
            above_segments.extend([segment(i, isolated_point) for i in intersections])
        else:
            above_segments.extend([segment(i, p) for i, p in zip(intersections, together_points)])
            above_segments.append(segment(*together_points))
        return above_segments


def binary_facet(fd):
    points = []
    for point_index in range(4):
        packed_coordinates = fd.read(3*4)
        if not packed_coordinates:
            raise IOError
        if point_index != 0:
            s = struct.Struct('3f')
            coordinates = s.unpack(packed_coordinates)
            points.append(point(*coordinates))
    fd.read(2)  # discard this field
    return facet(*points)

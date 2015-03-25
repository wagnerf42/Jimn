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
            if p.get_z() >= h:
                return False
        return True

    def is_above(self, h):
        for p in self.points:
            if p.get_z() <= h:
                return False
        return True

    def is_included(self, h):
        for p in self.points:
            if p.get_z() != h:
                return False
        return True

    def single_point_intersection(self, h):
        above, included, below = 0, 0, 0
        for p in self.points:
            if p.get_z() > h:
                above += 1
            if p.get_z() < h:
                below += 1
            if p.get_z() == h:
                included += 1
        if included == 1 and (above == 2 or below == 2):
            return True
        else:
            return False

    def separate(self, h):
        lower = []
        higher = []
        same = []
        for p in self.points:
            if p.get_z() < h:
                lower.append(p)  # TODO: inf_equal or sup_equal ?
            elif p.get_z() > h:
                higher.append(p)
            else:
                same.append(p)
        if len(higher) == 1:
            return lower+same, higher[0]
        else:
            return higher+same, lower[0]

    def intersect(self, h):
        if self.is_above(h):
            return []

        if self.is_below(h):
            return []

        if self.is_included(h):
            return []

        if self.single_point_intersection(h):
            return []

        # separate points above and below, alone or not
        together_points, isolated_point = self.separate(h)

        traversing_segments = [segment(p, isolated_point) for p in together_points]
        intersection_points = [s.intersect(h) for s in traversing_segments]

        intersection_segment = [segment(*intersection_points)]
        return intersection_segment


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

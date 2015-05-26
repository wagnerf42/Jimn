# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.point import point
from jimn.segment import segment
import struct


class facet:
    def __init__(self, *points):
        self.points = list(points)

    def __str__(self):
        return "[{}]".format(';'.join(map(lambda p: str(p), self.points)))

    def segments(self):
        p1, p2, p3 = self.points
        return [segment(p1, p2), segment(p1, p3), segment(p2, p3)]

    def add_point(self, p):
        self.points.append(p)

    def find_points_above_and_below(self, h):
        points = [[], []]
        for p in self.points:
            points[p.is_above(h)].append(p)
        return points

    def intersect(self, h):
        lower_points, higher_points = self.find_points_above_and_below(h)
        if len(lower_points) == 2:
            together_points = lower_points
            isolated_point = higher_points[0]
        elif len(higher_points) == 2:
            together_points = higher_points
            isolated_point = lower_points[0]
            if(isolated_point.get_z() == h):
                return []
        else:
            return []

        traversing_segments = [segment(p, isolated_point) for p in together_points]
        intersection_points = [s.intersect(h) for s in traversing_segments]

        intersection_segment = [segment(*intersection_points)]
        return intersection_segment


def binary_facet(fd):
    points = []
    max_height = float('-inf')
    min_height = float('+inf')
    for point_index in range(4):
        packed_coordinates = fd.read(3*4)
        if not packed_coordinates:
            raise IOError
        if point_index != 0:
            s = struct.Struct('3f')
            coordinates = s.unpack(packed_coordinates)
            height = coordinates[-1]
            if max_height < height:
                max_height = height
            if min_height > height:
                min_height = height
            points.append(point(*coordinates))
    fd.read(2)  # discard this field
    return (facet(*points), min_height, max_height)

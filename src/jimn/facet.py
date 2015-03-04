# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.point import point
import struct


class facet:
    def __init__(self, *points):
        self.points = [p for p in points]

    def __str__(self):
        return "[{}]".format(';'.join(map(lambda p: str(p), self.points)))

    def add_point(self, p):
        self.points.append(p)


def svg_facet(fd):
    for point_index in range(4):
        packed_coordinates = fd.read(3*4)
        if not packed_coordinates:
            raise IOError
        if point_index == 0:
            next  # discard normal
        s = struct.Struct('f3')
        point(s.unpack(packed_coordinates))
    fd.read(2)  # discard this field

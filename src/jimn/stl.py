# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.point import point
from jimn.facet import facet, binary_facet
import struct
import re


class stl:
    def __init__(self, file_name):
        self.facets = parse_stl(file_name)

    def horizontal_intersection(self, h):
        segments = []
        for t in self.facets:
            segments.extend(t.intersect(h))
        return segments


def projection2d(segments_set):
    return [s.projection2d() for s in segments_set]


def parse_stl(file_name):
    if binary_stl_header(file_name):
        return parse_binary_stl(file_name)
    else:
        return parse_ascii_stl(file_name)


def binary_stl_header(file_name):
    with open(file_name, "rb") as f:
        zeroes_head = f.read(80)
        if not zeroes_head:
            return False
        s = struct.Struct('80c')
        zeroes = s.unpack(zeroes_head)
        for h in zeroes:
            if h != b'\x00':
                return False
        return True


def parse_binary_stl(file_name):
    with open(file_name, "rb") as f:
        f.read(80)
        packed_size = f.read(4)
        if not packed_size:
            return False
        s = struct.Struct('I')
        size = s.unpack(packed_size)[0]
        facets = []
        for i in range(size):
            facets.append(binary_facet(f))
        return facets


def parse_ascii_stl(file_name):
    fd = open(file_name, "r")
    s = fd.read()
    fd.close()
    head, *facets_strings = s.split('facet normal')
    if not re.search('^solid\s+\S*', head):
        raise IOError
    facets = []
    for facet_string in facets_strings:
        normal, *points_strings = facet_string.split('vertex')
        if len(points_strings) != 3:
            raise IOError
        points = []
        for point_string in points_strings:
            m = re.search('^\s*(-?\d+(\.\d+)?)\s+(-?\d+(\.\d+)?)\s+(-?\d+(\.\d+)?)', point_string)
            points.append(point(float(m.group(1)), float(m.group(3)), float(m.group(5))))

        facets.append(facet(*points))

    return facets

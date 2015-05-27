# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from math import ceil
from jimn.point import point
from jimn.facet import facet, binary_facet
import struct
import re


class stl:
    def __init__(self, file_name):
        self.facets = []
        self.max_height = float('-inf')
        self.min_height = float('+inf')
        if __debug__:
            print('loading stl file')
        self.parse_stl(file_name)
        if __debug__:
            print('stl file loaded')

    def horizontal_intersection(self, h):
        segments = []
        remaining_facets = []
        for t in self.facets:
            kept_facet, intersected_segment = t.intersect(h)
            if kept_facet is not None:
                remaining_facets.append(kept_facet)
            if intersected_segment is not None:
                segments.append(intersected_segment)
        self.facets = remaining_facets
        return segments

    def compute_slices(self, slice_size):
        slices = []
        slices_number = ceil((self.max_height - self.min_height)/slice_size)
        for slice_number in range(slices_number):
            lower_boundary = self.max_height - (slice_number+1) * slice_size
            slices.append(projection2d(self.horizontal_intersection(lower_boundary)))
        return slices

    def parse_stl(self, file_name):
        if binary_stl_header(file_name):
            return self.parse_binary_stl(file_name)
        else:
            return self.parse_ascii_stl(file_name)

    def parse_binary_stl(self, file_name):
        with open(file_name, "rb") as f:
            f.read(80)
            packed_size = f.read(4)
            if not packed_size:
                return False
            s = struct.Struct('I')
            size = s.unpack(packed_size)[0]
            for i in range(size):
                (new_facet, min_height, max_height) = binary_facet(f)
                self.facets.append(new_facet)
                self.update_height_limits(min_height, max_height)

    def update_height_limits(self, min_height, max_height):
        if self.min_height > min_height:
            self.min_height = min_height
        if self.max_height < max_height:
            self.max_height = max_height

    def parse_ascii_stl(file_name):
        fd = open(file_name, "r")
        s = fd.read()
        fd.close()
        head, *facets_strings = s.split('facet normal')
        if not re.search('^solid\s+\S*', head):
            raise IOError
        for facet_string in facets_strings:
            normal, *points_strings = facet_string.split('vertex')
            if len(points_strings) != 3:
                raise IOError
            points = []
            for point_string in points_strings:
                m = re.search('^\s*(-?\d+(\.\d+)?)\s+(-?\d+(\.\d+)?)\s+(-?\d+(\.\d+)?)', point_string)
                (x, y, z) = (float(m.group(1)), float(m.group(3)), float(m.group(5)))
                self.update_height_limits(z, z)
                points.append(point(x, y, z))

            self.facets.append(facet(*points))


def projection2d(segments_set):
    return [s.projection2d() for s in segments_set]

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



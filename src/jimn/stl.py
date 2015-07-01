# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from math import ceil
from jimn.point import point
from jimn.segment import segment
from jimn.facet import facet, binary_facet
from jimn.bounding_box import bounding_box
import struct
import re


class stl:
    def __init__(self, file_name):
        self.facets = []
        self.bounding_box = bounding_box.empty_box(3)
        if __debug__:
            print('loading stl file')
        self.parse_stl(file_name)
        if __debug__:
            print('stl file loaded')

    def horizontal_intersection(self, h):
        segments = []
        remaining_facets = []
        for t in self.facets:
            t.intersect(h, segments, remaining_facets)
        self.facets = remaining_facets
        return segments

    def compute_slices(self, slice_size):
        slices = {}
        min_height, max_height = self.bounding_box.limits(2)
        slices_number = ceil((max_height - min_height)/slice_size)
        for slice_number in range(slices_number):
            lower_boundary = max_height - (slice_number+1) * slice_size
            current_slice = projection2d(self.horizontal_intersection(lower_boundary))
            slices[lower_boundary] = current_slice
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
            data = f.read(size*(4*3*4+2)) # read all file
            #  for each facet : 4 vectors of 3 floats + 2 unused bytes
            s = struct.Struct('12fh')
            for fields in s.iter_unpack(data):
                (new_facet, facet_bounding_box) = binary_facet(fields)
                self.facets.append(new_facet)
                self.bounding_box.update(facet_bounding_box)

    def parse_ascii_stl(file_name):
        with open(file_name, "r") as fd:
            s = fd.read()
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
                b = bounding_box([x, y, z], [x, y, z])
                self.bounding_box.update(b)
                points.append(point([x, y, z]))

            self.facets.append(facet(points))

    def border_2d(self):
        """returns list of 2d segments encompassing projection of stl"""
        # get coordinates
        xmin, xmax = self.bounding_box.limits(0)
        ymin, ymax = self.bounding_box.limits(1)
        # extend slightly border
        xmin = xmin - 0.01
        ymin = ymin - 0.01
        xmax = xmax + 0.01
        ymax = ymax + 0.01

        # build four points
        points = []
        points.append(point([xmin, ymin]))
        points.append(point([xmin, ymax]))
        points.append(point([xmax, ymax]))
        points.append(point([xmax, ymin]))
        points.append(points[0])

        # build four segments
        border_segments = []
        for i in range(4):
            s = segment([points[i], points[i+1]])
            border_segments.append(s.sort_endpoints())
        return border_segments

    def remove_non_vertical_facets(self):
        self.facets = [f for f in self.facets if f.is_vertical()]
    def keep_facets_near(self, p, limit):
        self.facets = [f for f in self.facets if f.is_near(p, limit)]

    def flatten(self):
        segments = []
        for f in self.facets:
            facet_segments = f.segments()
            for s in facet_segments:
                if not s.is_vertical_3d():
                    segments.append(s)
        segments2d = projection2d(segments)
        return [s.sort_endpoints() for s in segments2d]


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



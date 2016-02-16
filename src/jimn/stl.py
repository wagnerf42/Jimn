"""
stl files (basic files (no colours)).
both binary and ascii loaders
"""

import struct
import re
from math import ceil
from jimn.point import Point
from jimn.facet import Facet, binary_facet
from jimn.bounding_box import BoundingBox
from jimn.utils.coordinates_hash import CoordinatesHash
from jimn.utils.debug import is_module_debugged


class Stl:
    """
    stl files are a set of 3d facets
    """
    def __init__(self, file_name):
        self.heights_hash = CoordinatesHash(wanted_precision=5)
        self.facets = []
        self.bounding_box = BoundingBox.empty_box(3)
        if __debug__:
            if is_module_debugged(__name__):
                print('loading stl file')
        self.parse_stl(file_name)
        if __debug__:
            if is_module_debugged(__name__):
                print('stl file loaded')

    def horizontal_intersection(self, height, translation_vector):
        """
        intersect model at given height.
        update remaining facets to keep only facets below (or at) given
        height.
        """
        segments = []
        remaining_facets = []
        for facet in self.facets:
            facet.intersect(height, segments, remaining_facets,
                            translation_vector)
        self.facets = remaining_facets
        return segments

    def compute_slices(self, slice_size, translation_vector):
        """
        cut stl into set of horizontal 2d slices spaced by slice_size.
        also translate all points by given vector to end up with positive
        coordinates.
        """
        slices = {}
        min_height, max_height = self.bounding_box.limits(2)
        slices_number = ceil((max_height - min_height)/slice_size)
        for slice_number in range(slices_number):
            lower_boundary = max_height - (slice_number+1) * slice_size
            lower_boundary = self.heights_hash.hash_coordinate(lower_boundary)
            if lower_boundary < min_height + 0.01:
                lower_boundary = min_height + 0.01
            current_slice = self.horizontal_intersection(lower_boundary,
                                                         translation_vector)
            slices[lower_boundary] = current_slice
        return slices

    def parse_stl(self, file_name):
        """
        load stl file.
        detect file type and call appropriate loader
        """
        if _binary_stl_header(file_name):
            return self.parse_binary_stl(file_name)
        else:
            return self.parse_ascii_stl(file_name)

    def parse_binary_stl(self, file_name):
        """
        load binary stl file (basic)
        """
        with open(file_name, "rb") as stl_file:
            stl_file.read(80)
            packed_size = stl_file.read(4)
            if not packed_size:
                return False
            size_struct = struct.Struct('I')
            size = size_struct.unpack(packed_size)[0]
            data = stl_file.read(size*(4*3*4+2)) # read all file
            #  for each facet : 4 vectors of 3 floats + 2 unused bytes
            facet_struct = struct.Struct('12fh')
            for fields in facet_struct.iter_unpack(data):
                new_facet = binary_facet(fields,
                                         self.heights_hash, self.bounding_box)
                self.facets.append(new_facet)

    def parse_ascii_stl(self, file_name):
        """
        ascii stl files loader
        """
        with open(file_name, "r") as stl_file:
            whole_file = stl_file.read()
        head, *facets_strings = whole_file.split("facet normal")
        if not re.search(r"^solid\s+\S*", head):
            raise IOError
        self._parse_ascii_facets(facets_strings)

    def _parse_ascii_facets(self, facets_strings):
        """
        take a list of strings (each a stl ascii facet) and build stl
        """
        for facet_string in facets_strings:
            points_strings = facet_string.split("vertex")
            if len(points_strings) != 3 and len(points_strings) != 4:
                raise IOError
            self._parse_ascii_points(points_strings[-3:])

    def _parse_ascii_points(self, points_strings):
        points = []
        for point_string in points_strings:
            matches = re.search(
                r"^\s*(-?\d+(\.\d+)?)\s+(-?\d+(\.\d+)?)\s+(-?\d+(\.\d+)?)",
                point_string)
            coordinates = [
                float(matches.group(1)),
                float(matches.group(3)),
                float(matches.group(5))
            ]
            coordinates[2] = self.heights_hash.hash_coordinate(coordinates[2])

            point = Point(coordinates)
            self.bounding_box.add_point(point)
            points.append(point)

        self.facets.append(Facet(points))

    def translation_vector(self, margin):
        """
        return translation vector to apply to all points
        to have border min point in (0, 0).
        """
        xmin, ymin = self.bounding_box.min_coordinates[0:2]
        return Point([margin-xmin, margin-ymin])

    def dimensions(self, margin):
        """
        return width and height required.
        """
        # get coordinates
        xmin, xmax = self.bounding_box.limits(0)
        ymin, ymax = self.bounding_box.limits(1)
        # extend slightly border
        xmin = xmin - margin
        ymin = ymin - margin
        xmax = xmax + margin
        ymax = ymax + margin
        return (xmax-xmin, ymax-ymin)

    def flatten(self):
        """
        segments obtained when seeing self from above
        """
        segments = []
        for facet in self.facets:
            facet_segments = facet.segments()
            for segment in facet_segments:
                if not segment.is_vertical_3d():
                    segments.append(segment)
        segments2d = [s.projection(2) for s in segments]
        return [s.sort_endpoints() for s in segments2d]


def _binary_stl_header(file_name):
    """
    detect if given file is a binary stl file
    """
    with open(file_name, "rb") as stl_file:
        zeroes_head = stl_file.read(80)
        if not zeroes_head:
            return False
        header_struct = struct.Struct('80c')
        zeroes = header_struct.unpack(zeroes_head)
        for value in zeroes:
            if value != b'\x00':
                return False
        return True

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
from jimn.displayable import tycat

# some constants for more readable slicing events
# this number is used for sorting events
# at same height we start by ending below facets
# then starting above facets and then intersecting
START, INTERSECTION, END = 1, 2, 0

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

    def compute_slices(self, slice_size, translation_vector):
        """
        cut stl into set of horizontal 2d slices spaced by slice_size.
        also translate all points by given vector to end up with positive
        coordinates.
        """
        min_height, max_height = self.bounding_box.limits(2)
        events = []
        slices_number = ceil((max_height - min_height)/slice_size)
        for slice_number in range(slices_number):
            lower_boundary = max_height - (slice_number+1) * slice_size
            lower_boundary = self.heights_hash.hash_coordinate(lower_boundary)
            if lower_boundary < min_height + 0.01:
                lower_boundary = min_height + 0.01
            events.append((lower_boundary, INTERSECTION, None))
        for facet in self.facets:
            heights = [p.coordinates[2] for p in facet.points]
            events.append((min(heights), START, facet))
            events.append((max(heights), END, facet))

        events.sort(key=lambda t: t[0:2])
        return run_slicing_events(events, translation_vector)

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
            facet_struct = struct.Struct('12fh')
            for _ in range(size):
                data = stl_file.read(4*3*4+2)
                #  for each facet : 4 vectors of 3 floats + 2 unused bytes
                fields = facet_struct.unpack(data)
                new_facet = binary_facet(fields, self.heights_hash,
                                         self.bounding_box)
                if not new_facet.is_horizontal():
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


def run_slicing_events(events, translation_vector):
    """
    executes all events, adding, removing and intersecting facets
    """
    slices = dict()
    facets = set()
    for height, event_type, facet in events:
        if event_type == START:
            facets.add(facet)
        elif event_type == END:
            facets.remove(facet)
        else:
            segments = []
            for facet in facets:
                facet.intersect(height, segments, translation_vector)
            slices[height] = segments
            if is_module_debugged(__name__):
                print(height)
                # print("\n".join(str(f) for f in facets))
                tycat(segments)

    return slices

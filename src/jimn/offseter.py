from jimn.displayable import tycat
from jimn.debug import is_module_debugged
from jimn.iterators import all_two_elements
from jimn.coordinates_hash import coordinates_hash
from jimn.arc import arc

"""requires polygon to be oriented counter clockwise to carve the inside
and clockwise to carve the outside"""


class offseter:
    def __init__(self, radius, polygon):
        self.polygon = polygon
        self.radius = radius

    def raw_offset(self):
        raw_segments = [
            s.parallel_segment(self.radius) for s in self.polygon.segments()
        ]

        if __debug__:
            if is_module_debugged(__name__):
                print("unjoined raw segments")
                tycat(self.polygon, raw_segments)

        return self.join_raw_segments(raw_segments)

    def join_raw_segments(self, raw_segments):

        edge = []
        rounder = coordinates_hash(dimension=2)
        for s1, s2 in all_two_elements(raw_segments):
            i = s1.intersection_with(s2, rounder)
            edge.append(s1)
            if i is None:
                # add arc
                binding = arc(self.radius,
                              [s1.get_endpoint(1), s2.get_endpoint(0)])
                edge.append(binding)
            else:
                # stop at intersection
                s1.set_endpoint(1, i)
                s2.set_endpoint(0, i)
        if __debug__:
            if is_module_debugged(__name__):
                print("joined segments")
                tycat(self.polygon, edge)
        return edge


def raw_offset(radius, polygon_to_offset):
    o = offseter(radius, polygon_to_offset)
    segments = o.raw_offset()
    if len(segments) < 2:
        return []
    else:
        return segments


def offset_holed_polygon(radius, *polygons):
    segments = []
    for p in polygons:
        segments.extend(raw_offset(radius, p))
    tycat(segments)

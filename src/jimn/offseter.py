from jimn.segment import segment
from jimn.polygon import polygon
from jimn.displayable import tycat
from jimn.debug import is_module_debugged
from jimn.point import point
from jimn.precision import is_almost
from jimn.iterators import all_two_elements, all_three_elements
from math import pi, sin, cos

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

        return self.join_raw_segments()

    def join_raw_segments(self, raw_segments):

        edge = []
        for s1, s2 in all_two_elements(raw_segments):
            i = s1.intersection_with(s2)
            edge.append(s1)
            if i is None:
                # add arc
                print("TODO: arc")
            else:
                # stop at intersection
                s1.set_endpoint(1, i)
                s2.set_endpoint(0, i)
        if __debug__:
            if is_module_debugged(__name__):
                print("joined segments")
                tycat(self.polygon, edge)


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

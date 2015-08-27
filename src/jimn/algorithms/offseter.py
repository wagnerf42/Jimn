from jimn.displayable import tycat
from jimn.arc import arc
from jimn.ghost import ghost
from jimn.algorithms.sweeping_line_algorithms.sweeping_offseter_selection\
    import select_offseted_paths
from jimn.utils.coordinates_hash import coordinates_hash
from jimn.utils.debug import is_module_debugged
from jimn.utils.iterators import all_two_elements

"""requires polygon to be oriented counter clockwise to carve the inside
and clockwise to carve the outside"""


class offseter:
    def __init__(self, radius, polygon, rounder):
        self.polygon = polygon
        self.radius = radius
        self.rounder = rounder

    def raw_offset(self):
        raw_segments = [
            s.parallel_segment(self.radius, self.rounder)
            for s in self.polygon.segments()
        ]

        if __debug__:
            if is_module_debugged(__name__):
                print("unjoined raw segments")
                tycat(self.polygon, raw_segments)

        return self.join_raw_segments(raw_segments)

    def join_raw_segments(self, raw_segments):

        edge = []
        for s1, s2 in all_two_elements(raw_segments):
            i = s1.intersection_with_segment(s2, self.rounder)
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


def raw_offset(radius, polygon_to_offset, rounder):
    o = offseter(radius, polygon_to_offset, rounder)
    segments = o.raw_offset()
    if len(segments) < 2:
        return []
    else:
        return segments


def offset_holed_polygon(radius, *polygons):

    # fill rounder with all coordinates
    rounder = coordinates_hash(2)
    for p in polygons:
        p.round_points(rounder)

    g = ghost([])
    for p in polygons:
        g.extend(raw_offset(radius, p, rounder))

    g.remove_overlapping_segments()
    g = g.compute_self_elementary_paths()
    remaining_paths = select_offseted_paths(g.get_content())
    print("TODO: rebuild holed polygons")
    sys.exit()
    return remaining_paths

from jimn.displayable import tycat
from jimn.arc import arc
from jimn.pocket import pocket
from jimn.pocket.elementary_paths import pocket_elementary_paths
from jimn.pocket.builder import build_pockets
from jimn.algorithms.sweeping_line_algorithms.sweeping_offseter_selection\
    import select_offseted_paths
from jimn.utils.coordinates_hash import coordinates_hash
from jimn.utils.debug import is_module_debugged
from jimn.utils.iterators import all_two_elements
from collections import defaultdict

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


def _raw_offset(radius, polygon_to_offset, rounder):
    o = offseter(radius, polygon_to_offset, rounder)
    segments = o.raw_offset()
    if len(segments) < 2:
        return []
    else:
        return segments


def _merge_included_pockets(pockets):
    """
    test which pocket is included in which other
    included pockets are merged in the including one
    returns a set of independent pockets
    """
    included_pockets = defaultdict(list)
    for p1 in pockets:
        for p2 in pockets:
            if id(p1) != id(p2):
                if p2.is_included_in(p1):
                    included_pockets[id(p1)].append(p2)
                    break
        else:
            included_pockets[id(p1)].append(p1)

    disjoint_pockets = []
    for pockets in included_pockets.values():
        p = pockets.pop()
        for merged_pocket in pockets:
            p.extend(merged_pocket.get_content())
        disjoint_pockets.append(p)

    return disjoint_pockets


def offset_holed_polygon(radius, *polygons):
    """
    takes a holed polygon and routing radius
    removes non accessible edges and returns
    a set of disjoint pockets
    """

    # fill rounder with all coordinates
    rounder = coordinates_hash(2)
    for p in polygons:
        p.round_points(rounder)

    overall_pocket = pocket([])
    for p in polygons:
        overall_pocket.extend(_raw_offset(radius, p, rounder))

    overall_pocket.remove_overlapping_segments()
    overall_pocket = pocket_elementary_paths(overall_pocket)
    remaining_paths = select_offseted_paths(overall_pocket.get_content())
    pockets = build_pockets(remaining_paths)
    final_pockets = _merge_included_pockets(pockets)
    return final_pockets

from jimn.bounding_box import bounding_box
from jimn.displayable import tycat
from jimn.arc import arc
from jimn.pocket import pocket
from jimn.holed_pocket import holed_pocket
from jimn.pocket.elementary_paths import pocket_elementary_paths
from jimn.pocket.builder import build_pockets
from jimn.utils.coordinates_hash import rounder2d
from jimn.utils.debug import is_module_debugged
from jimn.utils.iterators import all_two_elements
from jimn.algorithms.sweeping_line_algorithms.sweeping_offsetter_selection \
    import select_offseted_paths
from collections import defaultdict

"""requires polygon to be oriented counter clockwise to carve the inside
and clockwise to carve the outside"""


class offsetter:
    def __init__(self, radius, polygon, rounder):
        self.polygon = polygon
        self.radius = radius
        self.rounder = rounder

    def raw_offset(self):
        raw_segments = [
            (s.parallel_segment(self.radius, self.rounder), s)
            for s in self.polygon.segments()
        ]

        if __debug__:
            if is_module_debugged(__name__):
                print("unjoined raw segments")
                segments = [t[0] for t in raw_segments]
                tycat(self.polygon, segments)

        return self.join_raw_segments(raw_segments)

    def join_raw_segments(self, raw_segments):

        edge = []
        for t1, t2 in all_two_elements(raw_segments):
            s1 = t1[0]  # get back displaced segments
            s2 = t2[0]
            i = s1.intersection_with_segment(s2, self.rounder)
            edge.append(s1)
            if i is None:
                # add arc
                center_point = t1[1].get_endpoint(1)  # rotate around orig point
                try:
                    binding = arc(self.radius,
                                  [s1.get_endpoint(1), s2.get_endpoint(0)],
                                  center_point)
                    binding.correct_endpoints_order()
                except:
                    print("failed joining segments")
                    tycat(self.polygon, center_point, s1, s2)
                    raise

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
    o = offsetter(radius, polygon_to_offset, rounder)
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
                if p1.is_included_in(p2):
                    included_pockets[id(p2)].append(p1)
                    break
        else:
            included_pockets[id(p1)].append(p1)

    disjoint_pockets = []
    for pockets in included_pockets.values():
        hp = holed_pocket(pockets[0])
        for i in range(1, len(pockets)):
            hp.add_inner_edge(pockets[i])
        disjoint_pockets.append(hp)

    return disjoint_pockets


def offset_holed_polygon(radius, *polygons):
    """
    takes a holed polygon and routing radius
    removes non accessible edges and returns
    a set of disjoint pockets
    """

    # fill rounder with all coordinates
    # quickly check we are not too small
    b = bounding_box.empty_box(2)
    for p in polygons:
        p.round_points(rounder2d)
        b.update(p.get_bounding_box())

    if b.diameter() < 2 * radius:
        return []

    overall_pocket = pocket([])
    for p in polygons:
        overall_pocket.extend(_raw_offset(radius, p, rounder2d))

    overall_pocket.remove_overlapping_segments()
    overall_pocket = pocket_elementary_paths(overall_pocket)
    if __debug__:
        if is_module_debugged(__name__):
            print("before path selection")
            tycat(overall_pocket)
    try:
        remaining_paths = select_offseted_paths(overall_pocket.get_content())
    except:
        print("failing paths selection for", radius, *polygons)
        tycat(*polygons)
        tycat(overall_pocket)
        raise

    if __debug__:
        if is_module_debugged(__name__):
            print("after path selection")
            tycat(remaining_paths)
    pockets = build_pockets(remaining_paths)
    final_pockets = _merge_included_pockets(pockets)
    if __debug__:
        if is_module_debugged(__name__):
            print("final pockets")
            tycat(final_pockets)
    return final_pockets

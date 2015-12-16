"""requires polygon to be oriented counter clockwise to carve the inside
and clockwise to carve the outside"""


class offsetter:
    def __init__(self, radius, polygon):
        self.polygon = polygon
        self.radius = radius

    def raw_offset(self):
        raw_segments = [
            (s.parallel_segment(self.radius, rounder2d), s)
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
            i = s1.intersection_with_segment(s2)
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
                i = rounder2d.hash_point(i)
                s1.set_endpoint(1, i)
                s2.set_endpoint(0, i)
        if __debug__:
            if is_module_debugged(__name__):
                print("joined segments")
                tycat(self.polygon, edge)
        return edge


def _offset(radius, polygon_to_offset):
    o = offsetter(radius, polygon_to_offset)
    edge = o.raw_offset()
    if len(edge) < 2:
        return []
    else:
        return pocket(edge)


def _merge_included_pockets(pockets):
    """
    test which pocket is included in which other
    included pockets are merged in the including one
    returns a set of independent pockets
    """
    included_pockets = defaultdict(list)
    for p in pockets:
        included_pockets[id(p)].append(p)

    for p1 in pockets:
        for p2 in pockets:
            if id(p1) != id(p2):
                if p1.is_included_in(p2):
                    assert len(included_pockets[id(p1)]) == 1
                    del included_pockets[id(p1)]
                    included_pockets[id(p2)].append(p1)
                    break

    disjoint_pockets = []
    for top_id, pockets in included_pockets.items():
        assert top_id == id(pockets[0])
        # discard holes here
        # we do that here and not before because they might now contain content
        # which will be discarded too
        if not pockets[0].is_oriented_clockwise():
            hp = holed_pocket(pockets[0], pockets[1:])
            disjoint_pockets.append(hp)

    return disjoint_pockets


def offset_holed_polygon(radius, *polygons):
    """
    takes a holed polygon and routing radius
    removes non accessible edges and returns
    a set of disjoint pockets
    """

    # offset each polygon
    pockets = [_offset(radius, p) for p in polygons]

    # remove overlapping segments
    for p1, p2 in combinations(pockets, r=2):
        p1.remove_overlap_with(p2)

    # compute intersections
    intersections = defaultdict(list) # to each path a list of intersections
    for p1, p2 in combinations(pockets, r=2):
        p1.intersections_with(p2, intersections)

    # compute self intersections and generate elementary paths
    paths = []
    for po in pockets:
        po.intersections_with(po, intersections)
        for p in po.paths:
            if id(p) in intersections:
                paths.extend(p.split_at(intersections[id(p)]))
            else:
                paths.append(p)

    if __debug__:
        if is_module_debugged(__name__):
            print("elementary paths")
            tycat(paths)

    try:
        pockets = build_pockets(paths, False)
    except:
        print("building pockets failed", *polygons)
        tycat(paths, *polygons)
        raise

    final_pockets = _merge_included_pockets(pockets)
    if __debug__:
        if is_module_debugged(__name__):
            print("final pockets")
            tycat(final_pockets)
    return final_pockets

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
from collections import defaultdict
from itertools import combinations

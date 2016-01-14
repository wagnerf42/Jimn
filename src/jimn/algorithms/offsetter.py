"""requires polygon to be oriented counter clockwise to carve the inside
and clockwise to carve the outside"""

from collections import defaultdict
from itertools import combinations
from jimn.displayable import tycat
from jimn.arc import Arc
from jimn.pocket import pocket
from jimn.holed_pocket import holed_pocket
from jimn.pocket.builder import build_pockets
from jimn.utils.coordinates_hash import rounder2d
from jimn.utils.debug import is_module_debugged
from jimn.utils.iterators import all_two_elements


class Offsetter:
    """
    holds all info during offsetting of a polygon.
    """
    def __init__(self, radius, polygon):
        self.polygon = polygon
        self.radius = radius

    def offset(self):
        """
        extend polygon's edges by moving parallel to them
        and reconnect pieces.
        """
        raw_segments = [
            (s.parallel_segment(self.radius).hash_endpoints(rounder2d), s)
            for s in self.polygon.segments()
        ]

        if __debug__:
            if is_module_debugged(__name__):
                print("unjoined raw segments")
                segments = [t[0] for t in raw_segments]
                tycat(self.polygon, segments)

        try:
            result = self.join_raw_segments(raw_segments)
            return result
        except:
            print("failed joining edges in offsetter")
            segments = [t[0] for t in raw_segments]
            tycat(self.polygon, segments)
            raise

    def join_raw_segments(self, raw_segments):
        """
        reconnect all parallel segments.
        """

        edge = []
        for neighbouring_tuples in all_two_elements(raw_segments):
            first_segment, second_segment = [p[0] for p in neighbouring_tuples]
            end = first_segment.endpoints[1]
            start = second_segment.endpoints[0]
            if end.is_almost(start):
                first_segment = first_segment.change_endpoint(1, start)
            else:
                # original point connecting the original segments
                center_point = neighbouring_tuples[0][1].endpoints[1]
                # add arc
                try:
                    binding = Arc(self.radius, [end, start], center_point)
                    binding.adjust_center()
                    binding.correct_endpoints_order()
                except:
                    print("failed joining segments")
                    tycat(self.polygon, center_point,
                          first_segment, second_segment)
                    raise

                edge.append(first_segment)
                edge.append(binding)

        if __debug__:
            if is_module_debugged(__name__):
                print("joined segments")
                tycat(self.polygon, edge)
        return edge


def _offset(radius, polygon_to_offset):
    offsetter = Offsetter(radius, polygon_to_offset)
    edge = offsetter.offset()
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
    for p in pockets:
        p.remove_overlap_with(p)

    # compute intersections
    intersections = defaultdict(list)  # to each path a list of intersections
    for p1, p2 in combinations(pockets, r=2):
        p1.intersections_with(p2, intersections)

    # compute self intersections and generate elementary paths
    for po in pockets:
        po.self_intersections(intersections)
        po.split_at(intersections)

    paths = []
    for po in pockets:
        paths.extend(po.paths)

    if __debug__:
        if is_module_debugged(__name__):
            print("elementary paths")
            tycat(paths)

    try:
        pockets = build_pockets(paths, False)
    except:
        tycat(paths, *polygons)
        raise

    final_pockets = _merge_included_pockets(pockets)
    if __debug__:
        if is_module_debugged(__name__):
            print("final pockets")
            tycat(final_pockets)
    return final_pockets

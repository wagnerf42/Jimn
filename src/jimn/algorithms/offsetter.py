"""requires polygon to be oriented counter clockwise to carve the inside
and clockwise to carve the outside"""

from collections import defaultdict, Counter
from itertools import combinations
from jimn.displayable import tycat
from jimn.arc import Arc
from jimn.segment import Segment
from jimn.pocket import Pocket
from jimn.holed_pocket import HoledPocket
from jimn.pocket.builder import build_pockets
from jimn.utils.coordinates_hash import ROUNDER2D
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
            (s.parallel_segment(self.radius).hash_endpoints(ROUNDER2D), s)
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
                first_segment = Segment([first_segment.endpoints[0], start])
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
        return Pocket(edge)


def _merge_included_pockets(pockets):
    """
    test which pocket is included in which other.
    included pockets are merged in the including one.
    discard toplevel holes and pockets included in several others.
    returns a set of independent pockets.
    """
    included_pockets = defaultdict(list)
    inclusions_count = Counter()
    toplevel_pockets = {}

    for pocket1 in pockets:
        toplevel_pockets[id(pocket1)] = pocket1
        for pocket2 in pockets:
            if id(pocket1) != id(pocket2):
                if pocket1.is_included_in(pocket2):
                    included_pockets[id(pocket2)].append(pocket1)
                    inclusions_count[id(pocket1)] += 1
                    if inclusions_count[id(pocket1)] > 1:
                        break  # no need to continue
                    if id(pocket1) in toplevel_pockets:
                        del toplevel_pockets[id(pocket1)]

    disjoint_pockets = []
    for toplevel_pocket in toplevel_pockets.values():
        # discard toplevel holes here
        # we do that here and not before because they might now contain content
        # which will be discarded too
        if not toplevel_pocket.is_oriented_clockwise():
            holes = [h for h in included_pockets[id(toplevel_pocket)]
                     if inclusions_count[id(h)] == 1]
            holed_pocket = HoledPocket(toplevel_pocket, holes)
            disjoint_pockets.append(holed_pocket)

    return disjoint_pockets


def offset_to_elementary_paths(radius, polygons):
    """
    compute all paths obtained when offsetting.
    handle overlaps and intersections and return
    a set of elementary paths ready to be used for
    rebuilding pockets.
    """
    # offset each polygon
    pockets = [_offset(radius, p) for p in polygons]

    # remove overlapping segments
    for pocket1, pocket2 in combinations(pockets, r=2):
        pocket1.remove_overlap_with(pocket2)
    for pocket in pockets:
        pocket.remove_overlap_with(pocket)

    # compute intersections
    intersections = defaultdict(list)  # to each path a list of intersections
    for pocket1, pocket2 in combinations(pockets, r=2):
        pocket1.intersections_with(pocket2, intersections)

    # compute self intersections and generate elementary paths
    for pocket in pockets:
        pocket.self_intersections(intersections)
        pocket.split_at(intersections)

    paths = []
    for pocket in pockets:
        paths.extend(pocket.paths)

    if __debug__:
        if is_module_debugged(__name__):
            print("elementary paths")
            tycat(paths)

    return paths


def offset_holed_polygon(radius, *polygons):
    """
    take a holed polygon and routing radius.
    remove non accessible surfaces and return
    a set of disjoint holed pockets.
    """

    paths = offset_to_elementary_paths(radius, polygons)

    try:
        pockets = build_pockets(paths)
    except:
        tycat(paths, *polygons)
        raise

    final_pockets = _merge_included_pockets(pockets)
    if __debug__:
        if is_module_debugged(__name__):
            print("final pockets")
            tycat(final_pockets)
    return final_pockets

from jimn.pocket import pocket
from jimn.displayable import tycat
from jimn.utils.iterators import two_arrays_combinations
from jimn.utils.coordinates_hash import rounder2d
from jimn.utils.debug import is_module_debugged
from collections import defaultdict
from itertools import combinations


def pocket_elementary_paths(raw_pocket):
    """brute force algorithm splitting all paths in pocket
    into elementary paths"""
    intersection_points = _find_new_points(
        raw_pocket,
        combinations(raw_pocket.paths, r=2)
    )
    return _split_paths_at(raw_pocket, intersection_points)


def pocket_intersect_paths(raw_pocket, intersecting_paths):
    """brute force algorithm splitting all paths in pocket by paths
    in intersecting_paths into elementary paths"""
    intersection_points = _find_new_points(
        raw_pocket,
        two_arrays_combinations(raw_pocket.paths, intersecting_paths)
    )
    return _split_paths_at(raw_pocket, intersection_points)


def _split_paths_at(raw_pocket, new_points):
    elementary_paths = []
    for p in raw_pocket.paths:
        if p not in new_points:
            elementary_paths.append(p)
        else:
            elementary_paths.extend(p.split_at(new_points[p]))
    return pocket(elementary_paths)


def _find_new_points(raw_pocket, paths_iterator):
    new_points = defaultdict(list)
    for p1, p2 in paths_iterator:
        intersections = p1.intersections_with(p2, rounder2d)
        for i in intersections:
            new_points[p1].append(i)
            new_points[p2].append(i)
            if __debug__:
                if is_module_debugged(__name__):
                    print("splitting here:")
                    tycat(raw_pocket, p1, i)
    return new_points

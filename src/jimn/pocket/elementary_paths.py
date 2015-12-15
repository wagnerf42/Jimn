from jimn.pocket import pocket
from jimn.displayable import tycat
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


def _split_paths_at(raw_pocket, new_points):
    elementary_paths = []
    for p in raw_pocket.paths:
        if p not in new_points:
            elementary_paths.append(p)
        else:
            try:
                elementary_paths.extend(p.split_at(new_points[p]))
            except:
                print("failed elementary paths on ", raw_pocket, "for", p)
                tycat(raw_pocket, p)
                raise
    return pocket(elementary_paths)


def _find_new_points(raw_pocket, paths_iterator):
    new_points = defaultdict(list)
    for p1, p2 in paths_iterator:
        intersections = p1.intersections_with(p2)
        for i in intersections:
            j = rounder2d.hash_point(i)
            new_points[p1].append(j)
            new_points[p2].append(j)
            if __debug__:
                if is_module_debugged(__name__):
                    print("splitting here:")
                    tycat(raw_pocket, p1, j)
    return new_points

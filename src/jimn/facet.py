# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.point import point
from jimn.segment import segment
from jimn.utils.precision import is_almost


class facet:
    """
    facet from stl file.
    3 3D points
    """
    def __init__(self, points):
        self.points = points

    def __str__(self):
        return "[{}]".format(';'.join(map(lambda p: str(p), self.points)))

    def segments(self):
        """
        returns the three segments forming the facet
        """
        p1, p2, p3 = self.points
        return [segment([p1, p2]), segment([p1, p3]), segment([p2, p3])]

    def is_vertical(self):
        """
        are we vertical (in 3d) ?
        """
        p1, p2, p3 = [p.projection2d() for p in self.points]
        return p1.is_aligned_with(p2, p3)

    def is_near(self, p, limit):
        """
        are we near a given point p ?
        useful to filter facets near a point generating bugs
        """
        points = [q.projection2d() for q in self.points]
        for p2 in points:
            if not p2.is_near(p, limit):
                return False
        return True

    def _find_points_above_and_below(self, h):
        """
        used in plane intersection
        """
        points = [[], []]
        for p in self.points:
            points[p.is_above(h)].append(p)
        return points

    def intersect(self, h, segments, remaining_facets):
        """intersect facet at height h
        if intersection is a segment add it to segments list
        if facet is not strictly above plane add it to remaining_facets
        for later use"""
        lower_points, higher_points = self._find_points_above_and_below(h)
        if len(higher_points) != 3:  # are we reused later ?
            remaining_facets.append(self)
        if len(lower_points) == 2:
            together_points = lower_points
            isolated_point = higher_points[0]
        elif len(higher_points) == 2:
            together_points = higher_points
            isolated_point = lower_points[0]
            if(is_almost(isolated_point.get_z(), h)):
                return
        else:
            return

        traversing_segments = [
            segment([p, isolated_point]) for p in together_points
        ]
        intersection_points = [
            s.horizontal_plane_intersection(h) for s in traversing_segments
        ]

        # because we round coordinates in intersection
        # it is possible that the two obtained points are now the same
        # check it to avoid creating a one point segment
        if intersection_points[0] == intersection_points[1]:
            return
        intersection_segment = segment(intersection_points)
        # sort endpoints for remaining algorithms
        segments.append(intersection_segment.sort_endpoints())


def binary_facet(all_coordinates, heights_hash, box):
    """parses a facet in a binary stl file.
    returns facet and its bounding box
    hashes all encountered heights in given coordinates hash
    """
    points = []
    for i in range(3):
        coordinates = list(all_coordinates[3+3*i:6+3*i])
        coordinates[2] = heights_hash.hash_coordinate(0, coordinates[2])
        p = point(coordinates)
        box.add_point(p)
        points.append(p)
    f = facet(points)
    return f

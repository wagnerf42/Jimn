"""
facet in stl file (three ordered 3d points)
"""
from jimn.point import Point
from jimn.segment import Segment
from jimn.utils.iterators import all_two_elements
from jimn.utils.precision import is_almost


class Facet:
    """
    facet in stl file (three ordered 3d points)
    """
    def __init__(self, points):
        self.points = points

    def __str__(self):
        points_strings = [str(p) for p in self.points]
        return "Facet([{}])".format(', '.join(points_strings))

    def segments(self):
        """
        returns the three segments forming the facet
        """
        return list(Segment([p, q]) for p, q in all_two_elements(self.points))

    def is_vertical(self):
        """
        are we vertical (in 3d) ?
        """
        point1, point2, point3 = [p.projection2d() for p in self.points]
        return point1.is_aligned_with(point2, point3)

    def _find_points_above_and_below(self, height):
        """
        used in plane intersection.
        return facet's points above given height
        and facet's points below
        """
        points = [[], []]
        for point in self.points:
            points[point.coordinates[2] > height].append(point)
        return points

    def intersect(self, height, segments, remaining_facets, translation_vector):
        """intersect facet at given height
        if intersection is a segment add it to segments list
        if facet is not strictly above plane add it to remaining_facets
        for later use"""
        lower_points, higher_points = self._find_points_above_and_below(height)
        if len(higher_points) != 3:  # are we reused later ?
            remaining_facets.append(self)
        if len(lower_points) == 2:
            together_points = lower_points
            isolated_point = higher_points[0]
        elif len(higher_points) == 2:
            together_points = higher_points
            isolated_point = lower_points[0]
            if is_almost(isolated_point.get_z(), height):
                return
        else:
            return

        traversing_segments = [
            Segment([p, isolated_point]) for p in together_points
        ]
        intersection_points = [
            s.horizontal_plane_intersection(height, translation_vector)
            for s in traversing_segments
        ]

        # because we round coordinates in intersection
        # it is possible that the two obtained points are now the same
        # check it to avoid creating a one point segment
        if intersection_points[0] == intersection_points[1]:
            return
        intersection_segment = Segment(intersection_points)
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
        coordinates[2] = heights_hash.hash_coordinate(coordinates[2])
        point = Point(coordinates)
        box.add_point(point)
        points.append(point)

    return Facet(points)

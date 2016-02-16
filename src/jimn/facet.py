"""
facet in stl file (three ordered 3d points)
"""
from jimn.point import Point
from jimn.segment import Segment
from jimn.utils.iterators import all_two_elements
from jimn.utils.precision import is_almost, check_precision
from jimn.utils.coordinates_hash import ROUNDER2D


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
        point1, point2, point3 = [p.projection(2) for p in self.points]
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
            segment_plane_intersection(s, height, translation_vector)
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


def __segment_intersection_at(self, intersecting_y):
    """
    return point on segment (self) at given y.
    precondition : y is valid height in segment.
    """
    (x_1, y_1), (x_2, y_2) = [p.coordinates for p in self.endpoints]
    if is_almost(x_1, x_2):
        return Point([x_1, intersecting_y])
    else:
        slope = (y_1 - y_2) / (x_1 - x_2)
        intersecting_x = (intersecting_y - y_1) / slope + x_1

    return Point([intersecting_x, intersecting_y])


def segment_plane_intersection(self, intersecting_z, translation_vector):
    """
    cut self (3d segment) with plane at given height.
    requires h between hmin and hmax of segment
    """
    p_1, p_2 = self.endpoints
    x_1, y_1, z_1 = p_1.coordinates
    x_2, y_2, z_2 = p_2.coordinates

    if __debug__:
        check_precision(z_1, z_2, 'horizontal_plane_intersection')

    intersecting_x = x_1 + (intersecting_z - z_1)/(z_2 - z_1)*(x_2 - x_1)
    intersecting_y = y_1 + (intersecting_z - z_1)/(z_2 - z_1)*(y_2 - y_1)
    # TODO: add conditionals for precision problems

    return ROUNDER2D.hash_point(Point([intersecting_x, intersecting_y])
                                + translation_vector)

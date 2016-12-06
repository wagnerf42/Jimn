"""
facet in stl file (three ordered 3d points)
"""
from jimn.point import Point
from jimn.segment import Segment
from jimn.utils.iterators import all_two_elements
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

    def is_horizontal(self):
        """
        returns if we are a horizontal facet
        """
        return self.points[0].coordinates[2] ==\
            self.points[1].coordinates[2] ==\
            self.points[2].coordinates[2]

    def segments(self):
        """
        returns the three segments forming the facet
        """
        return [Segment([p, q]) for p, q in all_two_elements(self.points)]

    def intersect(self, height, segments, translation_vector):
        """
        intersect facet at given height
        if intersection is a segment add it to segments list
        """
        intersections = []
        for start, end in all_two_elements(self.points):
            if (start.coordinates[2] == height) and (end.coordinates[2] == height):
                segment_start = ROUNDER2D.hash_point(
                    Point(start.coordinates[0:2]) + translation_vector)
                segment_end = ROUNDER2D.hash_point(
                    Point(end.coordinates[0:2]) + translation_vector)
                segments.append(Segment([segment_start, segment_end]).sort_endpoints())
                return
            intersection = segment_plane_intersection(start, end, height, translation_vector)
            if intersection is not None:
                intersections.append(intersection)

        if len(intersections) == 2:
            segments.append(Segment(intersections).sort_endpoints())


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


def segment_plane_intersection(p_1, p_2, intersecting_z, translation_vector):
    """
    cut 3d segment between p_1, p_2 with plane at given height.
    """
    x_1, y_1, z_1 = p_1.coordinates
    x_2, y_2, z_2 = p_2.coordinates

    if z_1 == z_2:
        return

    alpha = (intersecting_z - z_1) / (z_2 - z_1)
    if not 0 < alpha < 1:  # endpoints excluded
        return

    intersecting_x = x_1 + (intersecting_z - z_1)/(z_2 - z_1)*(x_2 - x_1)
    intersecting_y = y_1 + (intersecting_z - z_1)/(z_2 - z_1)*(y_2 - y_1)

    return ROUNDER2D.hash_point(Point([intersecting_x, intersecting_y]) +
                                translation_vector)

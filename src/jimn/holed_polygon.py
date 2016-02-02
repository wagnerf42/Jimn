"""
class for polygons with holes inside.
"""
from math import pi


class HoledPolygon:
    """
    polygons with holes inside. contains polygons without holes.
    """
    def __init__(self, polygon, height=None, holes=None):
        self.polygon = polygon
        assert not self.polygon.is_oriented_clockwise()
        if holes is None:
            self.holes = []
        else:
            self.holes = holes
            if __debug__:
                for hole in self.holes:
                    assert hole.is_oriented_clockwise()
        self.height = height

    def large_enough_for(self, milling_radius):
        """
        will anything be left if we mill with such a radius ?
        """
        min_required_area = pi * milling_radius * milling_radius
        return abs(self.polygon.area()) > min_required_area

    def get_dot_label(self):
        """
        returns text label for display in dot file (see polygontree class)
        """
        if not self.holes:
            string = "{}, h={}".format(
                str(self.polygon.get_label()),
                str(self.height)
            )
        else:
            string = "{}, h={}, holes={}".format(
                str(self.polygon.get_label()),
                str(self.height),
                str([h.label for h in self.holes])
            )

        return string

    def get_polygons(self):
        """
        returns a list of all polygons we contain
        (both outer edge and holes)
        """
        polygons = list(self.holes)
        polygons.append(self.polygon)
        return polygons

    def get_bounding_box(self):
        """
        min bounding box containing holed polygon.
        """
        return self.polygon.get_bounding_box()

    def save_svg_content(self, display, color):
        """
        svg for tycat.
        """
        self.polygon.save_svg_content(display, color)
        for hole in self.holes:
            hole.save_svg_content(display, color)

    def normalize(self):
        """
        prepares for hashing by reordering points and
        re-orienting
        """
        self.polygon.orient(clockwise=False)
        self.polygon.normalize_starting_point()
        for hole in self.holes:
            hole.orient(clockwise=True)
            hole.normalize_starting_point()
        self.holes = sorted(self.holes, key=lambda h: h.points[0])

    def translation_vector(self, other):
        """
        return translation vector from self to obtain other.
        'none' if impossible.
        requires holed polygon to be normalized.
        """
        if len(self.holes) != len(other.holes):
            return None

        diff_vector = self.polygon.translation_vector(other.polygon)
        if not diff_vector:
            return None
        for hole1, hole2 in zip(self.holes, other.holes):
            if not hole1.translation_vector(hole2, diff_vector):
                return False
        return diff_vector

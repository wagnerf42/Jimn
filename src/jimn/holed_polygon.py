class holed_polygon:
    def __init__(self, polygon, height=None, holes=None):
        self.polygon = polygon
        assert not self.polygon.is_oriented_clockwise()
        if holes is None:
            self.holes = []
        else:
            self.holes = holes
            if __debug__:
                for h in self.holes:
                    assert h.is_oriented_clockwise()
        self.height = height

    def large_enough_for(self, milling_radius):
        """
        will anything be left if we mill with such a radius ?
        """
        min_required_area = pi * milling_radius * milling_radius
        return abs(self.polygon.area()) > min_required_area

    def contains_holes(self):
        return len(self.holes) > 0

    def get_height(self):
        return self.height

    def get_dot_label(self):
        """
        returns text label for display in dot file (see polygontree class)
        """
        if not self.holes:
            return ("{}, h={}".format(
                str(self.polygon.get_label()),
                str(self.height))
            )
        else:
            return ("{}, h={}, holes={}".format(
                str(self.polygon.get_label()),
                str(self.height),
                str([h.label for h in self.holes]))
            )

    def get_polygons(self):
        """
        returns a list of all polygons we contain
        (both outer edge and holes)
        """
        polygons = list(self.holes)
        polygons.append(self.polygon)
        return polygons

    def get_bounding_box(self):
        return self.polygon.get_bounding_box()

    def save_svg_content(self, display, color):
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
        for h in self.holes:
            h.orient(clockwise=True)
            h.normalize_starting_point()
        self.holes = sorted(self.holes, key=lambda h: h.get_points()[0])

    def translation_vector(self, p2):
        """
        returns translation vector from self to obtain p2.
        'none' if impossible.
        """
        v = self.polygon.translation_vector(p2.polygon)
        if not v:
            return None
        if len(self.holes) != len(p2.holes):
            return None
        for h1, h2 in zip(self.holes, p2.holes):
            # TODO: is this really ok ??? -> only if holes are generated in
            # same order
            if not h1.translation_vector(h2, v):
                return False
        return v

    def round_points(self, rounder):
        """
        round all points through given rounder
        """
        self.polygon.round_points(rounder)
        for h in self.holes:
            h.round_points(rounder)

    def tycat(self, border):
        tycat(border, self.polygon, *(self.holes))

from jimn.displayable import tycat
from math import pi

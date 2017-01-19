"""
class for polygons with holes inside.
"""


class HoledPolygon:
    """
    polygons with holes inside. holes are basic polygons.

    for example:

    - create a square with a square hole inside:

    holed_poly = HoledPolygon(
        Polygon.square(0, 0, 10),
        [Polygon.square(3, 3, 5)]
    )

    """
    def __init__(self, polygon, holes=None, height=None):
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
        normalize all content for hashing by reordering points.
        """
        self.polygon.normalize_starting_point()
        for hole in self.holes:
            hole.normalize_starting_point()

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

    def __str__(self):
        holes = ",\n".join(str(h) for h in self.holes)
        return "HoledPolygon(" + str(self.polygon) \
            + ", holes=[" + holes + "])\n"

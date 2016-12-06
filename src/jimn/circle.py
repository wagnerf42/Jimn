"""
draw circles (mainly for debugging purposes).
"""
from jimn.point import Point
from jimn.bounding_box import BoundingBox


class Circle:
    """
    circle object (center + radius).
    """
    def __init__(self, center, radius):
        self.center = center
        self.radius = radius

    def save_svg_content(self, display):
        """
        svg content for tycat.
        """
        display.write('<circle cx="{}" cy="{}"'.format(*self.center.coordinates))
        display.write(' r="{}"/>'.format(self.radius * display.svg_stretch))

    def get_bounding_box(self):
        """
        min bounding box containing circle.
        """
        min_point = self.center + Point([-self.radius, -self.radius])
        max_point = self.center + Point([self.radius, self.radius])
        return BoundingBox(
            min_point.coordinates,
            max_point.coordinates,
        )

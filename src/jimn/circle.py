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

    def save_svg_content(self, display, color):
        """
        svg content for tycat.
        """
        svg_coordinates = display.convert_coordinates(
            self.center.coordinates
        )
        display.write("<circle cx=\"{}\" cy=\"{}\"".format(*svg_coordinates))
        display.write(" r=\"{}\" fill=\"none\" stroke=\"{}\"".format(
            self.radius * display.svg_stretch, color
        ))
        display.write(" opacity=\"0.5\" stroke-width=\"{}\"/>\n".format(
            display.stroke_width()
        ))

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

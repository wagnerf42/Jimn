class circle:
    def __init__(self, center, radius):
        self.center = center
        self.radius = radius

    def save_svg_content(self, display, color):
        svg_coordinates = display.convert_coordinates(
            self.center.get_coordinates()
        )
        display.write("<circle cx=\"{}\" cy=\"{}\"".format(*svg_coordinates))
        display.write(" r=\"{}\" fill=\"none\" stroke=\"{}\"".format(
            self.radius * display.stretch(), color
        ))
        display.write(" opacity=\"0.5\" stroke-width=\"{}\"/>\n".format(
            display.stroke_width()
        ))

    def get_bounding_box(self):
        min_point = self.center + Point([-self.radius, -self.radius])
        max_point = self.center + Point([self.radius, self.radius])
        return Bounding_Box(
            min_point.get_coordinates(),
            max_point.get_coordinates(),
        )

from jimn.point import Point
from jimn.bounding_box import Bounding_Box

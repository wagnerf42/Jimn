from jimn.bounding_box import bounding_box
from jimn.polygon import polygon

"""
set of paths defining a pocket to mill.
"""


class pocket:
    def __init__(self, paths):
        self.paths = paths

    def get_points(self):
        for p in self.paths:
            yield p.get_endpoint(0)

    def round_points(self, rounder):
        for p in self.paths:
            p.round_points(rounder)

    def to_polygon(self):
        return polygon(list(self.get_points()))

    def is_oriented_clockwise(self):
        return self.to_polygon().is_oriented_clockwise()

    def get_bounding_box(self):
        box = bounding_box.empty_box(2)
        for p in self.paths:
            box.update(p.get_bounding_box())
        return box

    def save_svg_content(self, display, color):
        for p in self.paths:
            p.save_svg_content(display, color)

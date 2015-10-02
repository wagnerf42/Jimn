class vertical_path:
    def __init__(self, direction):
        self.direction = direction

    def update_height(self, height):
        return self.direction + height

    def save_svg_content(self, display, color):
        return

    def get_bounding_box(self):
        return bounding_box.empty_box(2)

from jimn.bounding_box import bounding_box

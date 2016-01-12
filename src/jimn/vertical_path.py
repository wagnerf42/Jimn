class vertical_path:
    def __init__(self, direction):
        self.direction = direction

    def update_height(self, height):
        return self.direction + height

    def save_svg_content(self, display, color):
        return

    def get_bounding_box(self):
        return Bounding_Box.empty_box(2)

    def contains(self, p):
        """
        returns always false (real method is for non-vertical paths only)
        """
        return False

    def length(self):
        """
        return distance to cross path
        """
        return 0

from jimn.bounding_box import Bounding_Box

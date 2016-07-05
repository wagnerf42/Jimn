"""
vertical (elementary) path class.
"""
from jimn.bounding_box import BoundingBox


class VerticalPath:
    """
    vertical path. can be up or down.
    """
    milling_height = 0

    def __init__(self, direction):
        self.direction = direction

    def copy(self):
        """
        return a deepcopy of given vertical path.
        """
        return VerticalPath(self.direction)

    def update_height(self, height):
        """
        take a height before path and return height after path.
        """
        return self.direction * self.milling_height + height

    def save_svg_content(self, display, color):
        # pylint: disable=unused-argument, no-self-use
        """
        svg for tycat. does nothing.
        """
        return

    def get_bounding_box(self):
        # pylint: disable=no-self-use
        """
        bounding box. empty box.
        """
        return BoundingBox.empty_box(2)

    def contains(self, point):
        # pylint: disable=unused-argument, no-self-use
        """
        returns always false (real method is for non-vertical paths only)
        """
        return False

    def length(self):
        """
        return distance to cross path.
        """
        return self.milling_height

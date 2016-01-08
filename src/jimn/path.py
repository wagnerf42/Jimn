import os

"""
a path is a list of arcs or segments
"""

path_modulo = os.environ.get("JIMN_PATH_ANIMATION")
if path_modulo is None:
    path_modulo = 10
else:
    path_modulo = int(path_modulo)


class path:
    def __init__(self, elementary_paths):
        self.elementary_paths = elementary_paths

    def get_first_point(self):
        return self.elementary_paths[0].get_endpoint(0)

    def get_elementary_paths(self):
        return self.elementary_paths

    def translate(self, translation):
        """
        translates the whole path by a given translation vector.
        returns new path if obtained path is different and same path
        if translation vector is (0,0)
        """
        if translation.is_almost(Point([0, 0])):
            return self
        return path([p.translate(translation) for p in self.elementary_paths])

    def set_elementary_paths(self, elementary_paths):
        self.elementary_paths = elementary_paths

    def get_start(self):
        return self.elementary_paths[0].get_endpoint(0)

    def get_bounding_box(self):
        box = bounding_box.empty_box(2)
        for p in self.elementary_paths:
            box.update(p.get_bounding_box())
        return box

    def save_svg_content(self, display, color):
        self.get_start().save_svg_content(display, color)
        heights = defaultdict(list)
        current_height = 0
        for p in self.elementary_paths:
            current_height = p.update_height(current_height)
            heights[current_height].append(p)

        for height, paths in heights.items():
            new_color = display.get_color_after(color, height)
            for p in paths:
                p.save_svg_content(display, new_color)

    def length(self):
        return sum([p.length() for p in self.elementary_paths])

    def append(self, elementary_path):
        self.elementary_paths.append(elementary_path)

    def extend(self, paths):
        self.elementary_paths.extend(paths)

    def animate(self, *other_things):
        displayed = path([])
        for i, p in enumerate(self.elementary_paths):
            displayed.append(p)
            if (i+1) % path_modulo == 0:
                tycat(displayed, other_things)
        tycat(displayed, other_things)

    def find_position(self, p):
        """
        find first position of point p
        """
        for i, ep in enumerate(self.elementary_paths):
            if ep.contains(p):
                return path_position(ep, p, i)

        raise Exception("point not found in path")

    def change_starting_point(self, p):
        """
        change starting point of cycle path.
        takes a position as argument.
        """
        start = self.elementary_paths[:p.index]
        end = self.elementary_paths[p.index+1:]
        new_cycle = []
        before, after = self.elementary_paths[p.index].split_around(p.point)

        if after is not None:
            new_cycle.append(after)
        new_cycle.extend(end)
        new_cycle.extend(start)
        if before is not None:
            new_cycle.append(before)
        self.elementary_paths = new_cycle

    def get_dot_label(self):
        return str(id(self))

    def get_points(self):
        """
        iterates through all points.
        """
        for p in self.elementary_paths:
            yield p.get_endpoint(0)

    def __hash__(self):
        return hash(id(self))

    def __str__(self):
        path_strings = [str(p) for p in self.elementary_paths]
        return "path([\n    " + ",\n    ".join(path_strings) + "\n])"


from jimn.path_position import path_position
from jimn.bounding_box import bounding_box
from jimn.displayable import tycat
from jimn.point import Point
from collections import defaultdict

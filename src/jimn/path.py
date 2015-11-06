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

    def get_elementary_paths(self):
        return self.elementary_paths

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
            if i % path_modulo == 0:
                tycat(displayed, other_things)
        tycat(displayed, other_things)

    def last_intersection_with(self, other):
        """
        loop on self, finding the last elementary path (in current order)
        intersecting with other.
        for now brute force quadratic algorithm.
        returns the two indices of intersecting elementary paths together
        with the last intersection point.
        """
        for i, p in enumerate(self.elementary_paths):
            for j, p2 in enumerate(other.elementary_paths):
                intersections = p.intersections_with(p2)
                if intersections:
                    last_intersection = intersections[-1]
                    assert False  # TODO : check this is good
                    last_intersecting_indices = (i, j)
        return last_intersecting_indices, last_intersection

    def change_starting_point(self, p):
        """
        change starting point of cycle path.
        pre-requisite: given point is on path
        """
        if p.is_almost(self.get_start()):
            return

        for i, ep in enumerate(self.elementary_paths):
            if ep.contains(p):
                index = i
                break
        else:
            raise Exception("we do not contain given starting point")
        start = self.elementary_paths[:index]
        end = self.elementary_paths[index+1:]
        new_cycle = []
        before, after = self.elementary_paths[index].split_around(p)

        if after is not None:
            new_cycle.append(after)
        new_cycle.extend(end)
        new_cycle.extend(start)
        if before is not None:
            new_cycle.append(before)
        self.elementary_paths = new_cycle

    def get_dot_label(self):
        return str(id(self))

    def points(self):
        """
        iterates through all points.
        """
        for p in self.elementary_paths:
            yield p.get_endpoint(0)

    def nearest_point(self, p):
        """
        returns nearest point on self from given point p.
        """
        #TODO: real nearest point
        best_distance = float("+inf")
        for p2 in self.points():
            d = p.distance_to(p2)
            if d < best_distance:
                best_distance = d
                best_point = p2
        return best_point

    def nearest_points(self, other):
        """
        returns nearest point on self from other path.
        """
        #TODO: real nearests points
        best_distance = float("+inf")
        for p in self.points():
            for p2 in other.points():
                d = p.distance_to(p2)
                if d < best_distance:
                    best_distance = d
                    best_points = (p, p2)
        return best_points

    def __hash__(self):
        return hash(id(self))


from jimn.bounding_box import bounding_box
from jimn.displayable import tycat
from collections import defaultdict

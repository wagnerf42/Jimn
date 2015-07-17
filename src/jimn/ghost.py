from jimn.coordinates_hash import coordinates_hash
from jimn.bounding_box import bounding_box
from jimn.debug import is_module_debugged
from jimn.displayable import tycat
from collections import defaultdict
from jimn.iterators import two_arrays_combinations

"""unordered set of segments and arcs
corresponding to the dead remnants of a holed polygon"""


class ghost:
    def __init__(self, paths):
        self.paths = paths

    def get_content(self):
        return self.paths

    def get_bounding_box(self):
        if not self.paths:
            return
        box = bounding_box.empty_box(self.paths[0].dimension())
        for p in self.paths:
            small_box = p.get_bounding_box()
            box.update(small_box)
        return box

    def save_svg_content(self, display, color):
        for p in self.paths:
            p.save_svg_content(display, color)

    def compute_elementary_paths(self, intersecting_paths):
        """brute force algorithm splitting all into elementary paths"""
        intersection_points = self.find_new_points(intersecting_paths)
        return self.split_paths_at(intersection_points)

    def split_paths_at(self, new_points):
        elementary_paths = []
        for p in self.paths:
            if p not in new_points:
                elementary_paths.append(p)
            else:
                elementary_paths.extend(p.split_at(new_points[p]))
        return elementary_paths

    def find_new_points(self, intersecting_paths):
        new_points = defaultdict(list)
        rounder = coordinates_hash(dimension=2)
        for p1, p2 in two_arrays_combinations(self.paths, intersecting_paths):
            intersections = p1.intersections_with(p2, rounder)
            for i in intersections:
                new_points[p1].append(i)
                if __debug__:
                    if is_module_debugged(__name__):
                        print("splitting here:")
                        tycat(self, p1, i)
        return new_points

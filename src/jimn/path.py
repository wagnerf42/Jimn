from jimn.bounding_box import bounding_box
from jimn.displayable import tycat

"""
a path is a list of arcs or segments
"""


class path:
    def __init__(self, elementary_paths):
        self.elementary_paths = elementary_paths

    def get_elementary_paths(self):
        return self.elementary_paths

    def get_start(self):
        return self.elementary_paths[0].get_endpoint(0)

    def fuse_with(self, starts):
        """
        take a set of cycles, hashed by their starting points.
        fuse them inside us.
        this is the last step of an eulerian path algorithm.
        """
        final_paths = []
        ongoing_paths = self.elementary_paths
        while ongoing_paths:
            # move on ongoing path
            # reverse edges because we start from the end
            p = ongoing_paths.pop().reverse()
            final_paths.append(p)
            current_vertex = p.get_endpoint(1)
            if current_vertex in starts:
                cycles = starts[current_vertex]
                cycle = cycles.pop()
                ongoing_paths.extend(cycle.elementary_paths)
                if not cycles:
                    del starts[current_vertex]
        self.elementary_paths = final_paths

    def get_bounding_box(self):
        box = bounding_box.empty_box(2)
        for p in self.elementary_paths:
            box.update(p.get_bounding_box())
        return box

    def save_svg_content(self, display, color):
        for p in self.elementary_paths:
            p.save_svg_content(display, color)

    def length(self):
        return sum([p.length() for p in self.elementary_paths])

    def animate(self, *other_things):
        displayed_paths = []
        for p in self.elementary_paths:
            displayed_paths.append(p)
            tycat(displayed_paths, other_things)

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

    def set_starting_point(self, start, index):
        """
        pre-requisite: path is a cycle ; elementary_path at 'index'
        contains 'start'.
        change cycle starting point so that first point is 'start'
        """
        intersecting_path = self.elementary_paths[index]
        split_end, split_start = intersecting_path.split_at(start)
        assert False  # TODO check always good
        cycle_end, cycle_start = (
            self.elementary_paths[:index],
            self.elementary_paths[index+1:]
        )
        self.elementary_paths = [split_start]
        self.elementary_paths.extend(cycle_start)
        self.elementary_paths.extend(cycle_end)
        self.elementary_paths.append(split_end)

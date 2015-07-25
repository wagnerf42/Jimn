from jimn.bounding_box import bounding_box
from jimn.displayable import tycat

"""
a path is a list of arcs or segments
"""


class path:
    def __init__(self, elementary_paths):
        self.elementary_paths = elementary_paths

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

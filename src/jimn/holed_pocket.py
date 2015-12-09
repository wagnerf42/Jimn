"""
differentiate paths in a pocket between outer edge and inner edge.
"""


class holed_pocket:
    def __init__(self, outer_edge, inner_edges=None):
        self.outer_edge = outer_edge
        if inner_edges:
            self.inner_edges = inner_edges
        else:
            self.inner_edges = []

    def is_included_in(self, possible_includer):
        """
        are we included in other ?
        """
        return self.outer_edge.is_included_in(possible_includer.outer_edge)

    def subpockets(self):
        """
        iterates on both outer pocket and inner pockets
        """
        yield self.outer_edge
        for inner_edge in self.inner_edges:
            yield inner_edge

    def split_at_milling_points(self, milling_diameter):
        split_paths = []
        for p in self.subpockets():
            p.split_at_milling_points(milling_diameter, split_paths)
        return pocket(split_paths)

    def get_bounding_box(self):
        return self.outer_edge.get_bounding_box()

    def save_svg_content(self, displayer, color):
        self.outer_edge.save_svg_content(displayer, color)
        for inner_edge in self.inner_edges:
            inner_edge.save_svg_content(displayer, color)

    def __str__(self):
        # TODO : better indentation
        inner_strings = ["    " + str(p) for p in self.inner_edges]
        return "holed_pocket([\n    " + \
            ",\n    ".join([str(p) for p in self.outer_edge.paths]) \
            + "\n], [" + "\n".join(inner_strings) \
            + "])"

    def get_dot_label(self):
        """
        returns text label for display in dot file (see polygontree class)
        """
        return str(id(self))

from jimn.pocket import pocket

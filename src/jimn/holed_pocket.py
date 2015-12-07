"""
differentiate paths in a pocket between outer edge and inner edge.
"""


class holed_pocket:
    def __init__(self, outer_edge):
        self.outer_edge = outer_edge
        self.inner_edges = []

    def add_inner_edge(self, inner_edge):
        self.inner_edges.append(inner_edge)

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

from jimn.pocket import pocket

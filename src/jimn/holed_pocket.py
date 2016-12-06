"""
differentiate paths in a pocket between outer edge and inner edge.
"""
from jimn.pocket import Pocket


class HoledPocket:
    """
    pocket with (possibly) holes inside.
    always special operations on the outer edge.
    """
    def __init__(self, outer_edge, inner_edges=None):
        self.outer_edge = outer_edge
        if inner_edges:
            # sort holes to have a good hash and comparison
            self.inner_edges = sorted(inner_edges,
                                      key=lambda e: e.paths[0].endpoints[0])
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
        """
        split all paths inside pocket on milling lines.
        """
        split_paths = []
        for subpocket in self.subpockets():
            subpocket.split_at_milling_points(milling_diameter, split_paths)
        return Pocket(split_paths)

    def get_bounding_box(self):
        """
        min bounding box containing pocket.
        """
        return self.outer_edge.get_bounding_box()

    def save_svg_content(self, displayer):
        """
        svg content for tycat.
        """
        self.outer_edge.save_svg_content(displayer)
        for inner_edge in self.inner_edges:
            inner_edge.save_svg_content(displayer)

    def get_dot_label(self):
        """
        returns text label for display in dot file (see polygontree class)
        """
        return str(id(self))

    def __str__(self):
        inner = ",".join(str(p) for p in self.inner_edges)
        return "HoledPocket(" + str(self.outer_edge) + ", [" + inner + "])\n"

    def __hash__(self):
        """
        this hash is used to cache paths computed out of pockets
        since the same pocket can be found on several slices.
        requires all holes to be sorted.
        """
        all_edges = [self.outer_edge]
        all_edges.extend(self.inner_edges)
        hash_value = hash(tuple(all_edges))
        return hash_value

    def __eq__(self, other):
        """
        needed for the hash. requires all holes to be sorted.
        """
        if self.outer_edge != other.outer_edge:
            return False
        if len(self.inner_edges) != len(other.inner_edges):
            return False

        for self_edge, other_edge in zip(self.inner_edges, other.inner_edges):
            if self_edge != other_edge:
                return False
        return True

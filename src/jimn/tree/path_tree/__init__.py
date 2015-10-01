from jimn.tree import tree


class path_tree(tree):

    def __init__(self, path=None):
        self.content = path
        self.children = []

    @classmethod
    def build(cls, pockets, milling_radius):
        """
        converts a pocket_tree to a path_tree
        """
        return _pocket_node_to_path_node(pockets, milling_radius)

    def animate(self):
        for n in self.depth_first_exploration():
            if n.content is not None:
                n.content.animate()

    def global_path(self, milling_radius):
        """
        flattens the tree into final path
        """
        # start by computing toplevel tour
        toplevel_tour = self._compute_toplevel_tour()
        # now, process all subtrees
        for c in self.children:
            c._merge_paths(milling_radius)
        return self._merge_toplevel(toplevel_tour)

    def _merge_toplevel(self, toplevel_tour):
        """
        tour all points in tour.
        at each point (except first)
        we go down in subtree and back up to continue touring
        """
        final_paths = []
        for i in range(len(toplevel_tour)-1):
            start = toplevel_tour[i]
            end = toplevel_tour[i+1]
            if not start.is_almost(end):
                final_paths.append(segment([start, end]))
            # TODO: go down
            final_paths.extend(self.children[i].content.get_elementary_paths())
            # TODO: go back up
        # back to origin
        final_paths.append(segment([toplevel_tour[-1], toplevel_tour[0]]))
        return path(final_paths)

    def _merge_paths(self, milling_radius):
        """
        recursive merging of paths
        """
        # figure out on small paths where overlapping takes place
        # it will be way more faster than after we merged back
        # all subtrees
        positions = [
            overlap_exit_position(self.content, c.content, milling_radius)
            for c in self.children
        ]

        # sort children by positions
        # from last overlapping to first overlapping
        # in this way we can keep valid position as array indices :
        # when we will update merged_path, if we start adding steps
        # at the end then positions for adding steps at the beginning
        # will still be valid
        positions = self._sort_children_and_positions(positions)

        # recurse
        for c in self.children:
            c._merge_paths(milling_radius)
            # now, insert merged paths in main one
            for i, c in enumerate(self.children):
                merge_path(self.content, c.content, positions[i])

    def _sort_children_and_positions(self, positions):
        pairs = [
            (p, self.children[i])
            for i, p in enumerate(positions)
        ]
        sorted_pairs = sorted(pairs, key=lambda pair: pair[0], reverse=True)
        sorted_children = []
        sorted_positions = []
        for pair in sorted_pairs:
            sorted_positions.append(pair[0])
            sorted_children.append(pair[1])
            self.children = sorted_children
            return sorted_positions

    def _compute_toplevel_tour(self):
        """
        find cycle starting at origin and
        passing through one point of each toplevel pocket.
        returns list of 2d points.
        This will also sort all children by order of visit of the tour
        and change each cycle starting point as the visited point
        """
        print("TODO: TSP instead of dumb algorithm")
        points = [point([0, 0])]
        for c in self.children:
            points.append(c.content.get_start())
            return points


def _pocket_node_to_path_node(pocket_node, milling_radius):
    p = pocket_node.get_content()
    if p is None:
        path = None
    else:
        g = build_graph(p, milling_radius)
        if __debug__:
            if is_module_debugged(__name__):
                print("turned pocket")
                tycat(p)
                print("into graph")
                tycat(g)

        path = find_eulerian_cycle(g)

    path_node = path_tree(path)
    path_node.children = [
        _pocket_node_to_path_node(n, milling_radius)
        for n in pocket_node.get_children()
    ]
    return path_node

from jimn.displayable import tycat
from jimn.graph.eulerian_cycle import find_eulerian_cycle
from jimn.path import path
from jimn.tree.path_tree.path_merger import overlap_exit_position, merge_path
from jimn.point import point
from jimn.pocket.graph_builder import build_graph
from jimn.segment import segment
from jimn.utils.debug import is_module_debugged

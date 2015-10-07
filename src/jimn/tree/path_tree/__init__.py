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

    def animate(self, *things):
        for n in self.depth_first_exploration():
            if n.content is not None:
                n.content.animate(*things)

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
            final_paths.append(vertical_path(-1))
            final_paths.extend(self.children[i].content.get_elementary_paths())
            final_paths.append(vertical_path(1))
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
        o = point([0, 0])
        g = graph()
        children = {}  # record to which child each point belongs
        for c in self.children:
            end = c.content.nearest_point(o)
            children[end] = c
            g.add_edge_between(o, c.content, segment([o, end]))
        for c1, c2 in combinations(self.children, 2):
            p1 = c1.content
            p2 = c2.content
            start, end = p1.nearest_points(p2)
            children[start] = c1
            children[end] = c2
            g.add_edge_between(p1, p2, segment([start, end]))
        tycat(g)
        cycle = tsp(g)
        tour = _convert_cycle_to_tour(cycle, children, o)
        self.children = [children[p] for p in tour if p != o]
        return tour


def _convert_cycle_to_tour(cycle, children, origin):
    """
    loops on cycle ; keeps only one point for each sub_path :
    the first one encountered in each.
    also sets starting point as origin and for each sub path
    sets the starting point as the one visited.
    """
    tour_start = [origin]
    tour_end = []
    origin_seen = False
    seen_children = {}
    for step in [e.get_path().get_endpoints() for e in cycle]:
        for p in step:
            if p == origin:
                origin_seen = True
            else:
                child = children[p]
                if child not in seen_children:
                    seen_children[child] = True
                    child.content.change_starting_point(p)
                    if origin_seen:
                        tour_start.append(p)
                    else:
                        tour_end.append(p)

    tour_start.extend(tour_end)
    return tour_start


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

        path = cycle_to_path(find_eulerian_cycle(g))

    path_node = path_tree(path)
    path_node.children = [
        _pocket_node_to_path_node(n, milling_radius)
        for n in pocket_node.get_children()
    ]
    return path_node

from jimn.displayable import tycat
from jimn.graph import graph
from jimn.graph.eulerian_cycle import find_eulerian_cycle, cycle_to_path
from jimn.graph.tsp import tsp
from jimn.path import path
from jimn.tree.path_tree.path_merger import overlap_exit_position, merge_path
from jimn.point import point
from jimn.pocket.graph_builder import build_graph
from jimn.segment import segment
from jimn.utils.debug import is_module_debugged
from jimn.vertical_path import vertical_path
from itertools import combinations

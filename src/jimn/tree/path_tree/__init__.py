"""
path trees are created from pockets tree by milling pockets
"""
from copy import deepcopy
from itertools import combinations
from jimn.utils.points_containers import nearest_point, nearest_points
from jimn.displayable import tycat
from jimn.graph import Graph
from jimn.graph.eulerian_cycle import find_eulerian_cycle, cycle_to_path
from jimn.graph.tsp import tsp
from jimn.path import Path
from jimn.tree.path_tree.path_merger import overlap_exit_position, merge_path
from jimn.point import Point
from jimn.pocket.graph_builder import build_graph
from jimn.segment import Segment
from jimn.utils.debug import is_module_debugged
from jimn.vertical_path import VerticalPath
from jimn.tree import Tree

paths_cache = {}  # small cache to avoid recomputing paths for identical pockets


class path_tree(Tree):

    def __init__(self, path=None, old_pocket=None):
        super().__init__(path)
        self.old_pocket = old_pocket

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
        # switch back to real tree
        self = self.uncompress(Point([0, 0]))
        # start by computing toplevel tour
        if len(self.children) > 20:
            toplevel_tour = self._compute_toplevel_tour_fast()
        else:
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
                final_paths.append(Segment([start, end]))
            final_paths.append(VerticalPath(-1))
            final_paths.extend(self.children[i].content.elementary_paths)
            final_paths.append(VerticalPath(1))
        # back to origin
        final_paths.append(Segment([toplevel_tour[-1], toplevel_tour[0]]))
        return Path(final_paths)

    def _merge_paths(self, milling_radius):
        """
        recursive merging of paths
        """
        # figure out on small paths where overlapping takes place
        # it will be way more faster than after we merged back
        # all subtrees
        positions = []
        for c in self.children:
            try:
                p = overlap_exit_position(self.content, c.content,
                                          c.old_pocket,
                                          milling_radius)
            except:
                print("overlapping positions failed for",
                      self.content, c.content, c.old_pocket, milling_radius)
                tycat(self.content, c.content)
                raise
            positions.append(p)

        # sort children by positions
        # from last overlapping to first overlapping
        # in this way we can keep valid position as array indices :
        # when we will update merged_path, if we start adding steps
        # at the end then positions for adding steps at the beginning
        # will still be valid
        positions = self._sort_children_and_positions(positions)

        # change cycle starting point in each child
        for i, c in enumerate(self.children):
            c.content.change_starting_point(positions[i].inner_position)

        # recurse
        for c in self.children:
            c._merge_paths(milling_radius)

        # now, insert merged paths in main one
        for i, c in enumerate(self.children):
            try:
                merge_path(self.content, c.content, positions[i])
            except:
                print("merging failed")
                tycat(self.content, c.content, positions[i].inner_point,
                      positions[i].outer_point, positions[i].elementary_path)
                raise

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
        o = Point([0, 0])
        g = Graph()
        children = {}  # record to which child each point belongs
        for c in self.children:
            end = nearest_point(c.old_pocket, o)
            children[end] = c
            g.add_edge_between(o, c.content, Segment([o, end]))
        for c1, c2 in combinations(self.children, 2):
            p1 = c1.content
            p2 = c2.content
            start, end = nearest_points(c1.old_pocket, c2.old_pocket)
            children[start] = c1
            children[end] = c2
            g.add_edge_between(p1, p2, Segment([start, end]))
        cycle = tsp(g)
        tour = self._convert_cycle_to_tour(cycle, children, o)
        return tour

    def _compute_toplevel_tour_fast(self):
        """
        fast and dumb algorithm
        """
        return [c.content.get_first_point() for c in self.children]

    def _convert_cycle_to_tour(self, cycle, children, origin):
        """
        loops on cycle ; keeps only one point for each sub_path :
        the first one encountered in each.
        also sets starting point as origin and for each sub path
        sets the starting point as the one visited.
        """
        self.children = []
        children_end = []
        origin_seen = False
        seen_children = {}
        for step in [e.path.endpoints for e in cycle]:
            for p in step:
                if p == origin:
                    origin_seen = True
                else:
                    child = children[p]
                    if child not in seen_children:
                        seen_children[child] = True
                        if origin_seen:
                            self.children.append(child)
                        else:
                            children_end.append(child)

        self.children.extend(children_end)

        tour = [origin]
        previous_point = origin
        for c in self.children:
            next_point = nearest_point(c.content, previous_point)
            # TODO: do everything in one pass instead of two
            next_point_position = c.content.find_position(next_point)
            c.content.change_starting_point(next_point_position)
            tour.append(next_point)
            previous_point = next_point
        return tour

    def uncompress(self, translation):
        """
        initializes an uncompressed_tree out of a compressed one
        """

        if self.content is not None:
            translated_content = self.content.translate(translation)
            translated_pocket = self.old_pocket.translate(translation)
            new_node = path_tree(translated_content, translated_pocket)
        else:
            new_node = path_tree()

        # generate children
        for c in self.children:
            for t in c.translations:
                new_translation = t + translation
                new_node.children.append(c.uncompress(new_translation))

        if __debug__:
            if is_module_debugged(__name__):
                if self.content is None:
                    # toplevel node
                    print("decompressed path tree")
                    new_node.tycat()

        return new_node


def _pocket_node_to_path_node(pocket_node, milling_radius):
    global paths_cache

    p = pocket_node.get_content()
    if p is None:
        path = None
        outer_edge = None
    else:
        outer_edge = p.outer_edge
        if p in paths_cache:
            # TODO: careful with that stuff (what about orders in holed pockets)
            path = deepcopy(paths_cache[p])
        else:
            try:
                g = build_graph(p, 2*milling_radius, True)
                if __debug__:
                    if is_module_debugged(__name__):
                        print("turned pocket")
                        print(p)
                        tycat(p)
                        print("into graph")
                        tycat(g)
            except:
                print("failed building graph", p)
                tycat(p)
                raise

            path = cycle_to_path(find_eulerian_cycle(g))
            paths_cache[p] = path

    path_node = path_tree(path, outer_edge)
    path_node.copy_translations(pocket_node)
    path_node.children = [
        _pocket_node_to_path_node(n, milling_radius)
        for n in pocket_node.get_children()
    ]
    return path_node

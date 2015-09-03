from jimn.tree import tree
from jimn.pocket.graph_builder import build_graph
from jimn.graph.eulerian_cycle import find_eulerian_cycle
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged


class path_tree(tree):

    def __init__(self, path=None):
        self.initial_path = path
        self.children = []

    @classmethod
    def build(cls, pockets, milling_radius):
        """
        converts a pocket_tree to a path_tree
        """
        return _pocket_node_to_path_node(pockets, milling_radius)

    def animate(self):
        for n in self.depth_first_exploration():
            if n.initial_path is not None:
                n.initial_path.animate()

    def global_path(self):
        print("TODO")


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

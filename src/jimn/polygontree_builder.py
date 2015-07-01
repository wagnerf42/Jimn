# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.inclusion_tree_builder import inclusion_tree_builder
from jimn.polygontree import polygontree
from jimn.debug import is_module_debugged
from collections import defaultdict


class polygontree_builder:
    def __init__(self, slices_polygons):
        self.polygons = slices_polygons
        self.tree = polygontree()

    def build(self):
        inclusion_tree = inclusion_tree_builder(self.polygons).ascend_polygons()
        self.add_child_rec(self.tree, inclusion_tree)
        if __debug__:
            if is_module_debugged(__name__):
                self.tree.tycat()

    def add_child_rec(self, new_node, old_node):
        for old_child in old_node.get_children():
            if old_child.is_a_polygon():
                holes = [old_grandchild.get_polygon() for old_grandchild in old_child.get_children() if not old_grandchild.is_a_polygon()]
                new_child = new_node.add_child(old_child.get_polygon(), old_child.get_height(), holes)
                self.add_child_rec(new_child, old_child)


def build_tree(slices_polygons):
    tree_builder = polygontree_builder(slices_polygons)
    tree_builder.build()
    tree_builder.tree.normalize_polygons()
    original_polygons = defaultdict(list) # we need to store which polygons are translated and which are not
    tree_builder.tree.replace_translated_polygons(original_polygons)
    return tree_builder.tree


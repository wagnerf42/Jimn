from jimn.algorithms.sweeping_line_algorithms.inclusion_tree_builder\
    import build_inclusion_tree
from jimn.polygontree import polygontree
from jimn.utils.debug import is_module_debugged
from collections import defaultdict


class polygontree_builder:
    def __init__(self, slices_polygons):
        self.polygons = slices_polygons
        self.tree = polygontree()

    def build(self):
        inclusion_tree = build_inclusion_tree(self.polygons)
        inclusion_tree.ascend_polygons()
        convert_inclusion_tree(self.tree, inclusion_tree)
        if __debug__:
            if is_module_debugged(__name__):
                self.tree.tycat()


def convert_inclusion_tree(polygon_tree_node, inclusion_tree_node):
    # inclusion tree contains only polygons (marked as holes or polygons)
    # we build a polygontree which contains holed polygons
    # so we need to called holed_polygon constructor
    for inclusion_tree_child in inclusion_tree_node.get_children():
        if inclusion_tree_child.is_a_polygon():
            # call holed_polygon constructor
            # first, get the holes to put inside
            holes = [
                grandchild.get_polygon()
                for grandchild in inclusion_tree_child.get_children()
                if not grandchild.is_a_polygon()
            ]
            polygon_tree_child = polygon_tree_node.add_child(
                inclusion_tree_child.get_polygon(),
                inclusion_tree_child.get_height(),
                holes
            )
            convert_inclusion_tree(polygon_tree_child, inclusion_tree_child)


def build_tree(slices_polygons):
    tree_builder = polygontree_builder(slices_polygons)
    tree_builder.build()
    tree_builder.tree.normalize_polygons() # needed for tree compression and offsetting
    #TODO : compress tree
    return tree_builder.tree

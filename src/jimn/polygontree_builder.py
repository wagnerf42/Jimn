# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.inclusion_tree_builder import inclusion_tree_builder
from jimn.polygontree import polygontree


class polygontree_builder:
    def __init__(self, slices_polygons):
        self.polygons = slices_polygons
        self.tree = polygontree()

        self.build()

    def build(self):
        super_tree = inclusion_tree_builder(self.polygons).ascend_polygons()
        self.tree.add_child_rec(super_tree)
        self.tree.tycat()

# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.inclusion_tree_builder import inclusion_tree_builder
from jimn.polygontree import polygontree
from jimn.debug import is_module_debugged


class polygontree_builder:
    def __init__(self, slices_polygons):
        self.polygons = slices_polygons
        self.tree = polygontree()

        self.build()

    def build(self):
        super_tree = inclusion_tree_builder(self.polygons).ascend_polygons()
        self.tree.add_child_rec(super_tree)
        if __debug__:
            if is_module_debugged(__name__):
                self.tree.tycat()

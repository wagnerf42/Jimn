from jimn.algorithms.sweeping_line_algorithms.inclusion_tree_builder\
    import build_inclusion_tree
from jimn.holed_polygon import holed_polygon
from jimn.tree import tree
from jimn.utils.debug import is_module_debugged


class polygon_tree(tree):

    @classmethod
    def build(cls, polygons):
        inclusion_tree = build_inclusion_tree(polygons)
        inclusion_tree.ascend_polygons()
        poly_tree = cls()
        _convert_inclusion_tree(poly_tree, inclusion_tree)
        if __debug__:
            if is_module_debugged(__name__):
                poly_tree.tycat()
        poly_tree.normalize_polygons()
        # TODO: compress tree
        return poly_tree

    def add_child(self, polygon, height, holes):
        new_child = polygon_tree(holed_polygon(polygon, height, holes))
        self.children.append(new_child)
        return new_child

    def normalize_polygons(self):
        """call normalize method on each polygon of the tree
        this is a prerequisite for translated polygon identifications
        """
        for node in self.depth_first_exploration():
            new_polygon = node.content
            if new_polygon is not None:
                new_polygon.normalize()


def _convert_inclusion_tree(polygon_tree_node, inclusion_tree_node):
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
            _convert_inclusion_tree(polygon_tree_child, inclusion_tree_child)

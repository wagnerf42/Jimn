from jimn.algorithms.sweeping_line_algorithms.inclusion_tree_builder\
    import build_inclusion_tree
from jimn.holed_polygon import holed_polygon
from jimn.tree import tree
from jimn.utils.debug import is_module_debugged


class polygon_tree(tree):

    @classmethod
    def build(cls, polygons):
        """
        figures out which polygon is included in which other.
        returns tree of all holed polygons to mill such that
        each node contains a holed polygon (except root) and
        each node cannot be milled before any of its ancestors.
        """
        inclusion_tree = build_inclusion_tree(polygons)
        inclusion_tree.ascend_polygons()
        poly_tree = cls()
        _convert_inclusion_tree(poly_tree, inclusion_tree)
        if __debug__:
            if is_module_debugged(__name__):
                poly_tree.tycat()
        poly_tree.normalize_polygons()
        poly_tree.compress()
        return poly_tree

    def find_translation(self, candidate_originals):
        """
        looks if content is translation of one of the original nodes (content).
        if yes returns corresponding original node and translation vector.
        """
        for c in candidate_originals:
            v = c.content.translation_vector(self.content)
            if v:
                return (c, v)
        return (None, None)

    def is_translation_of(self, other, vector):
        """
        returns true if applying translation of given vector to
        all content of other gives self
        """
        #TODO
        return True

    def add_translation(self, translation_vector):
        """
        mark node (and whole subtree) as duplicated for
        given translation vector.
        """
        self.translations.append(translation_vector)

    def compress(self):
        """
        find in the tree if some subtrees are the translation of
        some others. add to each node a list of translations
        to apply and cut away all redundant branches.
        """
        older_brothers = []
        for c in self.children:
            c.compress()
            translated_brother, translation_vector = \
                c.find_translation(older_brothers)
            keep_child = True
            if translation_vector:
                keep_child = not c.is_translation_of(
                    translated_brother,
                    translation_vector)
            if keep_child:
                older_brothers.append(c)
            else:
                translated_brother.add_translation(translation_vector)
        self.children = older_brothers

    def add_child(self, polygon, height, holes):
        new_child = polygon_tree(holed_polygon(polygon.orient(False),
                                               height, holes))
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
                grandchild.get_polygon().orient(True)
                for grandchild in inclusion_tree_child.get_children()
                if not grandchild.is_a_polygon()
            ]
            polygon_tree_child = polygon_tree_node.add_child(
                inclusion_tree_child.get_polygon(),
                inclusion_tree_child.get_height(),
                holes
            )
            _convert_inclusion_tree(polygon_tree_child, inclusion_tree_child)

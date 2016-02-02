"""
tree of holed polygons.
"""
from jimn.algorithms.sweeping_line_algorithms.inclusion_tree_builder\
    import build_inclusion_tree
from jimn.holed_polygon import HoledPolygon
from jimn.tree import Tree
from jimn.utils.debug import is_module_debugged
from jimn.caching import cached_args


class PolygonTree(Tree):
    """
    tree of holed polygons.
    """

    @classmethod
    def build(cls, milling_radius, polygons):
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
                print("initial holed polygon tree")
                poly_tree.tycat()
        poly_tree.prune(milling_radius)
        if __debug__:
            if is_module_debugged(__name__):
                print("pruned holed polygon tree")
                poly_tree.tycat()
        poly_tree.normalize_polygons()
        poly_tree.compress()
        return poly_tree

    def prune(self, milling_radius):
        """
        remove all holed_polygons too small to be carved.
        """
        remaining_children = [
            c for c in self.children
            if c.content.large_enough_for(milling_radius)
        ]
        self.children = remaining_children
        for child in self.children:
            child.prune(milling_radius)

    def find_translation(self, candidate_originals):
        """
        look if content is translation of one of the original nodes (content).
        if yes returns corresponding original node and translation vector.
        """
        for node in candidate_originals:
            vector = node.content.translation_vector(self.content)
            if vector:
                return (node, vector)
        return (None, None)

    @cached_args
    def is_translation_of(self, other, vector):
        """
        return true if applying translation of given vector to
        all content of other gives self.
        """
        if len(self.children) != len(other.children):
            return False

        difference = self.content.translation_vector(other.content)
        if not difference.is_almost(vector):
            return False

        for my_child, his_child in zip(self.children, other.children):
            if not my_child.is_translation_of(his_child, vector):
                return False

        return True

    def compress(self):
        """
        find in the tree if some subtrees are the translation of
        some others. add to each node a list of translations
        to apply and cut away all redundant branches.
        """
        older_brothers = []
        for child in self.children:
            child.compress()
            translated_brother, translation_vector = \
                child.find_translation(older_brothers)
            keep_child = True
            if translation_vector:
                keep_child = not child.is_translation_of(
                    translated_brother,
                    translation_vector)
            if keep_child:
                older_brothers.append(child)
            else:
                translated_brother.add_translation(translation_vector)
        self.children = older_brothers

    def add_child(self, polygon, height, holes):
        """
        add given holed polygon as child.
        """
        new_child = PolygonTree(HoledPolygon(polygon.orient(False),
                                             height, holes))
        self.children.append(new_child)
        return new_child

    def normalize_polygons(self):
        """
        call normalize method on each polygon of the tree.
        this is a prerequisite for translated polygon identifications.
        """
        for child in self.children:
            child.normalize_polygons()

        # sort children
        self.children = sorted(self.children,
                               key=lambda c: c.content.polygon.points[0])

        if self.content is not None:
            self.content.normalize()

    def __eq__(self, other):
        """
        used in caches. we only need to compare ids.
        """
        return id(self) == id(other)

    def __hash__(self):
        """
        used in caches. we only need to hash id.
        """
        return hash(id(self))


def _convert_inclusion_tree(polygon_tree_node, inclusion_tree_node):
    # inclusion tree contains only polygons (marked as holes or polygons)
    # we build a polygontree which contains holed polygons
    # so we need to call holed_polygon constructor
    for inclusion_tree_child in inclusion_tree_node.children:
        if inclusion_tree_child.is_polygon:
            # call holed_polygon constructor
            # first, get the holes to put inside
            holes = [
                grandchild.content.orient(True)
                for grandchild in inclusion_tree_child.get_children()
                if not grandchild.is_polygon
            ]
            polygon_tree_child = polygon_tree_node.add_child(
                inclusion_tree_child.content,
                inclusion_tree_child.height,
                holes
            )
            _convert_inclusion_tree(polygon_tree_child, inclusion_tree_child)

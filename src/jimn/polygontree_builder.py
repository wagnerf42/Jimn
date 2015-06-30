# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.inclusion_tree_builder import inclusion_tree_builder
from jimn.polygontree import polygontree
from jimn.debug import is_module_debugged
from itertools import combinations


class polygontree_builder:
    def __init__(self, slices_polygons):
        self.polygons = slices_polygons
        self.tree = polygontree()

        self.build()

    def build(self):
        inclusion_tree = inclusion_tree_builder(self.polygons).ascend_polygons()
        self.add_child_rec(self.tree, inclusion_tree)
        if __debug__:
            if is_module_debugged(__name__):
                self.tree.tycat()

    def normalize(self):
        self.normalize_rec(self.tree)

    def normalize_rec(self, node):
        new_polygon = node.holed_polygon
        if new_polygon is not None:
            new_polygon.normalize()
        for c in node.children:
            self.normalize_rec(c)

    # assumes holed_polygons in tree are normalized
    def translated_holed_polygons(self):
        polygons = {}
        self.translated_holed_polygons_rec(self.tree, polygons)
        translated_polygons = []
        indexes = {}
        for degree, same_degree_polygons in polygons.items():
            for p1, p2 in combinations(same_degree_polygons, 2):
                if p1.is_translated(p2):

                    if p1 in indexes:
                        if p2 not in indexes:
                            k = indexes[p1]
                            translated_polygons[k].append(p2)
                            indexes[p2] = k
                    elif p2 in indexes:
                        k = indexes[p2]
                        translated_polygons[k].append(p1)
                        indexes[p1] = k
                    else:
                        translated_polygons.append([p1, p2])
                        k = len(translated_polygons) - 1
                        indexes[p1] = k
                        indexes[p2] = k

        return translated_polygons

    def translated_holed_polygons_rec(self, node, polygons):
        new_polygon = node.holed_polygon
        if new_polygon is not None:
            degree = len(new_polygon.polygon.get_points())
            if degree not in polygons:
                polygons[degree] = [new_polygon]
            else:
                polygons[degree].append(new_polygon)
        for c in node.children:
            self.translated_holed_polygons_rec(c, polygons)

    def add_child_rec(self, new_node, old_node):
        for old_child in old_node.get_children():
            if old_child.is_a_polygon():
                holes = [old_grandchild.get_polygon() for old_grandchild in old_child.get_children() if not old_grandchild.is_a_polygon()]
                new_child = new_node.add_child(old_child.get_polygon(), old_child.get_height(), holes)
                self.add_child_rec(new_child, old_child)

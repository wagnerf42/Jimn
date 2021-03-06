"""
this file allows an inclusion_tree creation.
it creates a 'raw' tree of polygons included one into another
in fast time.
no distinction between holes and filled_spaces now.
distinction will be made later when converting to polygon_tree.
"""
from jimn.tree.translated_tree import TranslatedTree
from jimn.polygon import Polygon


class InclusionTree(TranslatedTree):
    """
    store a set of polygons included one inside another.
    """
    def __init__(self, contained_polygon=None, height=None):
        super().__init__(contained_polygon)
        self.height = height

    def remove_children(self):
        """
        delete all children.
        """
        self.children = []

    def add_child(self, new_segment):
        """"
        add new child with polygon contained in provided segment.
        """
        new_polygon = new_segment.polygon
        height = new_segment.height
        leaf = InclusionTree(new_polygon, height)
        self.children.append(leaf)
        return leaf

    def ascend_polygons(self, father=None, grandfather=None):
        """
        recursively works on the whole subtree.
        for each hole, move its children to its grand father.
        """
        raise Exception("TODO")


def __polygon_label(self):
    """
    return label of polygon for interactive display in tree.
    """
    return str(id(self))

setattr(Polygon, 'get_dot_label', __polygon_label)

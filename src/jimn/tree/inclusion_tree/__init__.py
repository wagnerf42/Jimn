"""
this file allows an inclusion_tree creation.
it creates a 'raw' tree of polygons included one into another
in fast time.
no distinction between holes and filled_spaces now.
distinction will be made later when converting to polygon_tree.
"""
from jimn.tree import Tree


# while scanning through the planes we encounter and leave polygons.
# polygons currently intersecting sweeping line are marked active
ALIVE = 0
DEAD = 1


class InclusionTree(Tree):
    """
    store a set of polygons included one inside another.
    """
    def __init__(self, contained_polygon=None, height=None, father=None):
        super().__init__(contained_polygon)
        # polygons not closed yet
        self.alive_children = {}
        self.height = height
        if father is not None:
            self.is_polygon = self.compute_polygonality(father)

    def compute_polygonality(self, father):
        """
        compute if we are a polygon
        or a hole.
        """
        if father.content is None:  # father is root node
            return True
        else:
            if __debug__:
                if not father.is_polygon:
                    assert father.height == self.height
            return (not father.is_polygon) or father.height > self.height

    def remove_children(self):
        """
        delete all children.
        """
        self.children = []
        self.alive_children = {}

    def add_child(self, new_segment):
        """"
        add new alive child with polygon contained in provided segment.
        """
        new_polygon = new_segment.polygon
        height = new_segment.height
        leaf = InclusionTree(new_polygon, height, self)
        self.children.append(leaf)
        self.alive_children[id(new_polygon)] = leaf
        return leaf

    def ascend_polygons(self, father=None, grandfather=None):
        """
        recursively works on the whole subtree.
        for each hole, move its children to its grand father.
        """
        for child in self.children:
            child.ascend_polygons(self, father)

        if (self.content is not None) and not self.is_polygon:
            grandfather.children.extend(self.children)
            self.remove_children()

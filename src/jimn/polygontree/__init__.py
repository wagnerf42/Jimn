from jimn.algorithms.offsetter import offset_holed_polygon
from jimn.holed_polygon import holed_polygon
from jimn.displayable import tycat
from jimn.polygontree.tree import tree


class polygontree(tree):
    def __init__(self, content=None):
        self.content = content
        self.children = []

    def add_child(self, polygon, height, holes):
        new_child = polygontree(holed_polygon(polygon, height, holes))
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

    def offset_polygons(self, carving_radius):
        """walks the tree, offsetting all polygons.
        since offsetting can cut polygons into sub-polygons, this will
        modify the tree in place.
        """
        # start with children
        subtrees = []
        for n in self.get_children():
            pockets = n.offset_polygons(carving_radius)
            subtrees.extend(pockets)

        if self.content is not None:
            # now, offset ourselves (if we are not root)
            polygons = self.content.get_polygons()
            pockets = offset_holed_polygon(carving_radius, *polygons)
            return _rebuild_offsetted_tree(pockets, subtrees)
        else:
            root = polygontree()
            root.children = subtrees
            return root

    def compute_path(self):
        print("TODO")


def _rebuild_offsetted_tree(pockets, subtrees):
    """
    takes a set of trees from sublevel and a set of pockets
    at current level ;
    figures out in which pocket each tree is included
    and rebuilds a global tree
    """
    new_trees = {}
    for p in pockets:
        new_trees[id(p)] = polygontree(p)

    for t in subtrees:
        for p in pockets:
            if t.content.is_included_in(p):
                new_trees[id(p)].children.append(t)
                break
        else:
            tycat(t.content, p)
            raise Exception("subtree does not belong here")

    return list(new_trees.values())



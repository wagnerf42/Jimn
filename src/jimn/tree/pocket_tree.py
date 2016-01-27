"""
tree of all pockets to mill.
"""
from jimn.algorithms.offsetter import offset_holed_polygon
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.tree import Tree


class PocketTree(Tree):
    """
    tree of all pockets to mill.
    """

    @classmethod
    def build(cls, poly_tree, carving_radius):
        """
        walk the polygon tree, offsetting all polygons.
        return pockets tree.
        """
        return _offset_polygons(poly_tree, carving_radius)

    def is_empty(self):
        """
        return if no pockets are left.
        """
        return len(self.children) == 0


def _offset_polygons(poly_tree, carving_radius):
    if __debug__:
        if is_module_debugged(__name__):
            print("building pockets tree from")
            poly_tree.tycat()
    # start with children
    subtrees = []
    for child in poly_tree.get_children():
        pockets = _offset_polygons(child, carving_radius)
        for pocket in pockets:
            pocket.copy_translations(child)
        subtrees.extend(pockets)

    holed_poly = poly_tree.content
    if holed_poly is not None:
        # now, offset ourselves (if we are not root)
        polygons = holed_poly.get_polygons()
        pockets = offset_holed_polygon(carving_radius, *polygons)

        if __debug__:
            if is_module_debugged(__name__):
                print("offsetting")
                tycat(polygons)
                print("into")
                tycat(pockets)
        return _build_offsetted_tree(pockets, subtrees)
    else:
        root = PocketTree()
        root.children = subtrees
        return root


def _build_offsetted_tree(pockets, subtrees):
    """
    takes a set of trees from sublevel and a set of pockets
    at current level ;
    figures out in which pocket each tree is included
    and rebuilds a global tree
    """
    new_trees = {}
    for pocket in pockets:
        new_trees[id(pocket)] = PocketTree(pocket)

    for node in subtrees:
        for pocket in pockets:
            if node.content.is_included_in(pocket):
                new_trees[id(pocket)].children.append(node)
                break
        else:
            tycat(node.content, *pockets)
            raise Exception("subtree does not belong here")

    return list(new_trees.values())

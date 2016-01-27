from jimn.algorithms.offsetter import offset_holed_polygon
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.tree import Tree


class pocket_tree(Tree):

    @classmethod
    def build(cls, poly_tree, carving_radius):
        """walks the polygon tree, offsetting all polygons.
        returns pockets tree.
        """
        return _offset_polygons(poly_tree, carving_radius)

    def is_empty(self):
        return len(self.children) == 0


def _offset_polygons(poly_tree, carving_radius):
    if __debug__:
        if is_module_debugged(__name__):
            print("building pockets tree from")
            poly_tree.tycat()
    # start with children
    subtrees = []
    for n in poly_tree.get_children():
        pockets = _offset_polygons(n, carving_radius)
        for p in pockets:
            p.copy_translations(n)
        subtrees.extend(pockets)

    holed_poly = poly_tree.get_content()
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
        root = pocket_tree()
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
    for p in pockets:
        new_trees[id(p)] = pocket_tree(p)

    for t in subtrees:
        for p in pockets:
            if t.content.is_included_in(p):
                new_trees[id(p)].children.append(t)
                break
        else:
            tycat(t.content, *pockets)
            raise Exception("subtree does not belong here")

    return list(new_trees.values())

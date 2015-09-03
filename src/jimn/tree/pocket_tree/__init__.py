from jimn.algorithms.offsetter import offset_holed_polygon
from jimn.displayable import tycat
from jimn.tree import tree


class pocket_tree(tree):

    @classmethod
    def build(cls, poly_tree, carving_radius):
        """walks the polygon tree, offsetting all polygons.
        returns pockets tree.
        """
        return _offset_polygons(poly_tree, carving_radius)


def _offset_polygons(poly_tree, carving_radius):
    # start with children
    subtrees = []
    for n in poly_tree.get_children():
        pockets = _offset_polygons(n, carving_radius)
        subtrees.extend(pockets)

    content = poly_tree.get_content()
    if content is not None:
        # now, offset ourselves (if we are not root)
        polygons = content.get_polygons()
        pockets = offset_holed_polygon(carving_radius, *polygons)
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
            tycat(t.content, p)
            raise Exception("subtree does not belong here")

    return list(new_trees.values())

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


# TODO: move alive+dead to children (like in tree) with alive hash

class InclusionTree(Tree):
    """
    store a set of polygons included one inside another.
    """
    def __init__(self, contained_polygon=None, height=None, father=None):
        self.polygon = contained_polygon
        self.height = height
        self.children = [None, None]
        # by separating between polygons still active or not we can speedup
        # inclusion tests
        self.children[ALIVE] = []
        self.children[DEAD] = []
        if father is not None:
            self.is_polygon = self.compute_polygonality(father)

    def compute_polygonality(self, father):
        """
        compute if we are a polygon
        or a hole.
        """
        if father.is_root():
            return True
        else:
            if __debug__:
                if not father.is_polygon:
                    assert father.height == self.height
            return (not father.is_polygon) or father.height > self.height

    def is_root(self):
        """
        true for root node only.
        """
        return self.polygon is None

    def get_children(self):
        """
        return all children of given node.
        """
        return self.children[ALIVE] + self.children[DEAD]

    def get_alive_children(self):
        """
        return all children for which segments are still to
        come when continuing scanning the plane.
        """
        return self.children[ALIVE]

    def get_dead_children(self):
        """
        return all children for which no segments are left to
        come when continuing scanning the plane.
        """
        return self.children[DEAD]

    def remove_children(self):
        """
        delete all children.
        """
        self.children[ALIVE] = []
        self.children[DEAD] = []

    def add_child(self, new_segment):
        """"
        add new alive child with polygon contained in provided segment.
        """
        new_polygon = new_segment.polygon
        height = new_segment.height
        leaf = InclusionTree(new_polygon, height, self)
        self.children[ALIVE].append(leaf)
        return leaf

    def kill_child(self, polygon_id):
        """
        move a child from alive list to dead list.
        """
        for k, child in enumerate(self.children[ALIVE]):
            if id(child.polygon) == polygon_id:
                to_kill_index = k
                break
        else:
            raise "no one here"
        killed = self.children[ALIVE].pop(to_kill_index)
        self.children[DEAD].append(killed)

    def save_dot(self, fd):
        """
        save dot content if given node in dot given file.
        """
        if self.polygon is None:
            fd.write("n{} [label=\"None\"];\n".format(id(self)))
        else:
            fd.write("n{} [label=\"{}, h={}\"];\n".format(
                id(self), str(self.polygon.label), str(self.height)))
        for child in self.children[ALIVE] + self.children[DEAD]:
            if child is not None:
                fd.write("n{} -> n{};\n".format(id(self), id(child)))

    def ascend_polygons(self, father=None, grandfather=None):
        """
        recursively works on the whole subtree.
        for each hole, move its children to its grand father.
        """
        children = self.get_children()
        for child in children:
            child.ascend_polygons(self, father)

        if not self.is_root() and not self.is_polygon:
            grandfather.get_dead_children().extend(self.get_children())
            self.remove_children()

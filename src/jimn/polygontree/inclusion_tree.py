from jimn.polygontree.tree import tree
import os
import getpass


"""This file is only used for polygontree creation
No distinction between holes and filled_spaces now
Distinction will be made later when building final tree"""

dot_count = 0

# while scanning through the planes we encounter and leave polygons.
# polygons currently intersecting sweeping line are marked active
ALIVE = 0
DEAD = 1


class inclusion_tree(tree):
    """stores a set of polygons included one inside another"""
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
        # compute if we are a polygon
        if father.is_root():
            return True
        else:
            if __debug__:
                if not father.is_polygon:
                    assert father.height == self.height
            return (not father.is_polygon) or father.height > self.height

    def is_root(self):
        return self.polygon is None

    def get_polygon(self):
        return self.polygon

    def get_children(self):
        return self.children[ALIVE] + self.children[DEAD]

    def get_alive_children(self):
        return self.children[ALIVE]

    def get_dead_children(self):
        return self.children[DEAD]

    def remove_children(self):
        self.children[ALIVE] = []
        self.children[DEAD] = []

    def get_height(self):
        return self.height

    def is_a_polygon(self):
        return self.is_polygon

    """"add new node with polygon contained in provided segment"""
    def add_child(self, new_segment):
        new_polygon = new_segment.get_polygon()
        height = new_segment.get_height()
        leaf = inclusion_tree(new_polygon, height, self)
        self.children[ALIVE].append(leaf)
        return leaf

    """move a child from alive list to dead list"""
    def kill_child(self, polygon_id):
        for k, c in enumerate(self.children[ALIVE]):
            if id(c.get_polygon()) == polygon_id:
                to_kill_index = k
                break
        else:
            raise "no one here"
        killed = self.children[ALIVE].pop(to_kill_index)
        self.children[DEAD].append(killed)

    def tycat(self):
        global dot_count
        user = getpass.getuser()
        directory = "/tmp/{}".format(user)
        if not os.path.exists(directory):
            os.makedirs(directory)
        dot_file = "{}/{}.dot".format(directory, dot_count)
        svg_file = "{}/{}.svg".format(directory, dot_count)
        dot_count = dot_count + 1
        dot_fd = open(dot_file, 'w')
        dot_fd.write("digraph g {\n")
        for node in self.depth_first_exploration():
            node.save_dot(dot_fd)
        dot_fd.write("}")
        dot_fd.close()
        os.system("dot -Tsvg {} -o {}".format(dot_file, svg_file))
        os.system("tycat {}".format(svg_file))

    def save_dot(self, fd):
        if self.polygon is None:
            fd.write("n{} [label=\"None\"];\n".format(id(self)))
        else:
            fd.write("n{} [label=\"{}, h={}\"];\n".format(id(self), str(self.polygon.label), str(self.height)))
        for child in self.children[ALIVE] + self.children[DEAD]:
            if child is not None:
                fd.write("n{} -> n{};\n".format(id(self), id(child)))

    """nodes inside a hole are moved up one level"""
    def ascend_polygons(self, father=None, grandfather=None):
        if self.is_root() or self.is_a_polygon():
            children = self.get_children()
        else:
            children = self.get_children()
            grandfather.get_dead_children().extend(children)
            self.remove_children()
        for c in self.get_children():
            c.ascend_polygons(self, father)

# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

import os
import getpass


dot_count = 0

"""This file is only used for polygontree creation
No distinction between holes and filled_spaces now
Distinction will be made later when building final tree"""


class inclusion_tree:
    """stores a set of polygons included one inside another"""
    def __init__(self, contained_polygon=None, height=None, father=None):
        if father is None:
            self.is_polygon = True
        else:
            if not father.is_polygon:
                assert father.height == height
            self.is_polygon = (not father.is_polygon) or father.height > height
        self.polygon = contained_polygon
        self.height = height
        self.children = []

    def get_polygon(self):
        return self.polygon

    def get_children(self):
        return self.children

    def remove_children(self):
        self.children = []

    def get_height(self):
        return self.height

    def is_a_polygon(self):
        return self.is_polygon

#    def try_insertion(self, builder, height):
#        if is_included(seg, self.polygon, current_segments):  # TODO: mettre is_included comme methode de tree_builder
#            if self.is_polygon or s.get_height() == self.height:
#                self.add_child(new_polygon, s.get_height())
#
#                return True
#        return False

    def add_child(self, new_polygon, height):
        leaf = inclusion_tree(new_polygon, height, self)
        self.children.append(leaf)

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
        self.save_dot(dot_fd)
        dot_fd.write("}")
        dot_fd.close()
        os.system("dot -Tsvg {} -o {}".format(dot_file, svg_file))
        os.system("tycat {}".format(svg_file))

    def save_dot(self, fd):
        fd.write("n{} [label=\"{}, h={}\"];\n".format(id(self), str(self.polygon.label), str(self.height)))
        for child in self.children:
            if child is not None:
                fd.write("n{} -> n{};\n".format(id(self), id(child)))
                child.save_dot(fd)

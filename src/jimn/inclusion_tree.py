# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.inclusion_tree_builder import is_included
import os
import getpass


dot_count = 0

"""This file is only used for polygontree creation
No distinction between holes and filled_spaces now
Distinction will be made later when building final tree"""


class inclusion_tree:
    """stores a set of polygons included one inside another"""
    def __init__(self, contained_polygon=None, height=None, is_polygon=None):
        # si mon père est un trou (forcèment au même niveau), je suis un polygone
        # sinon, si mon père est à un niveau supérieur, je suis un polygone
        # sinon, je suis un trou
        self.polygon = contained_polygon
        self.is_polygon = is_polygon
        self.height = height
        self.children = []

    def add_polygon(self, new_polygon, seg, current_segments):
        if self.polygon is None:
            self.polygon = new_polygon
            self.is_polygon = True
            self.height = seg.get_height()
        else:
            self.add_polygon_rec(new_polygon, seg, current_segments)

    def add_polygon_rec(self, new_polygon, seg, current_segments):
        if not is_included(seg, self.polygon, current_segments):
            return False
        else:
            # TODO: explain why sorted
            for c in sorted(self.children, key=lambda c: c.height, reverse=True):
                if c.add_polygon_rec(new_polygon, seg, current_segments):
                    return True
            # TODO: condition supplémentaire
            # on ajoute que si :
            # - self est un polygone
            # ou
            # - (self est un trou) mais au même niveau
            if self.is_polygon or self.height == seg.get_height():
                if not self.is_polygon:
                    assert self.height == seg.get_height()
                child_is_polygon = (not self.is_polygon) or self.height > seg.get_height()
                self.add_child(new_polygon, seg.get_height(), child_is_polygon)
                return True
            else:
                return False

    def add_child(self, new_polygon, height, is_polygon):
        leaf = inclusion_tree(new_polygon, height, is_polygon)
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

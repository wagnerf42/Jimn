# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.holed_polygon import holed_polygon
import os
import getpass

dot_count = 0


class polygontree:
    def __init__(self, holed_polygon=None):
        self.holed_polygon = holed_polygon
        self.children = []

    def add_child_rec(self, current_node):
        for c in current_node.get_children():
            if c.is_polygon:
                holes = [gc.polygon for gc in c.get_children() if not gc.is_polygon]
                new_child = polygontree(holed_polygon(c.polygon, c.height, holes))
                self.children.append(new_child)
                new_child.add_child_rec(c)

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
        if self.holed_polygon is None:
            fd.write("n{} [label=\"None\"];\n".format(id(self)))
        elif self.holed_polygon.holes == []:
            fd.write("n{} [label=\"{}, h={}\"];\n".format(id(self), str(self.holed_polygon.polygon.label), str(self.holed_polygon.height)))
        else:
            fd.write("n{} [label=\"{}, h={}\nholes={}\"];\n".format(id(self), str(self.holed_polygon.polygon.label), str(self.holed_polygon.height), str([h.label for h in self.holed_polygon.holes])))
        for child in self.children:
            if child is not None:
                fd.write("n{} -> n{};\n".format(id(self), id(child)))
                child.save_dot(fd)

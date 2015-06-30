# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.holed_polygon import holed_polygon
from queue import Queue
import os
import getpass

dot_count = 0


class polygontree:
    def __init__(self, holed_polygon=None):
        self.holed_polygon = holed_polygon
        self.children = []

    def add_child(self, polygon, height, holes):
        new_child = polygontree(holed_polygon(polygon, height, holes))
        self.children.append(new_child)
        return new_child

    def display_depth_first(self):
        border = self.children[0].holed_polygon.polygon
        self.display_depth_first_rec(border)

    def display_depth_first_rec(self, border):
        if self.holed_polygon is not None:
            print("displaying polygon {}, h={}".format(self.holed_polygon.polygon.label, self.holed_polygon.height))
            print("holes = {}".format(str([h.label for h in self.holed_polygon.holes])))
            self.holed_polygon.tycat(border)
        for c in self.children:
            c.display_depth_first_rec(border)

    def display_breadth_first(self):
        border = self.children[0].holed_polygon.polygon
        to_display = Queue()
        to_display.put(self)
        while not to_display.empty():
            node = to_display.get()
            if node.holed_polygon is not None:
                print("displaying polygon {}, h={}".format(node.holed_polygon.polygon.label, node.holed_polygon.height))
                print("holes = {}".format(str([h.label for h in node.holed_polygon.holes])))
                node.holed_polygon.tycat(border)
            for c in node.children:
                to_display.put(c)

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

# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.holed_polygon import holed_polygon
from jimn.translated_holed_polygon import translated_holed_polygon
from queue import Queue
from queue import LifoQueue
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

    def depth_first(self):
        q = LifoQueue()
        q.put(self)
        while not q.empty():
            node = q.get()
            yield node
            for c in node.children:
                q.put(c)

    def breadth_first(self):
        q = Queue()
        q.put(self)
        while not q.empty():
            node = q.get()
            yield node
            for c in node.children:
                q.put(c)

    def display_depth_first(self):
        border = self.children[0].holed_polygon.polygon
        for node in self.depth_first():
            if node.holed_polygon is not None:
                node.tycat()
                node.holed_polygon.tycat(border)

    def display_breadth_first(self):
        border = self.children[0].holed_polygon.polygon
        for node in self.breadth_first():
            if node.holed_polygon is not None:
                node.tycat()
                node.holed_polygon.tycat(border)

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
        elif not self.holed_polygon.holes:
            fd.write("n{} [label=\"{}, h={}\"];\n".format(id(self), str(self.holed_polygon.polygon.label), str(self.holed_polygon.height)))
        else:
            fd.write("n{} [label=\"{}, h={}\nholes={}\"];\n".format(id(self), str(self.holed_polygon.polygon.label), str(self.holed_polygon.height), str([h.label for h in self.holed_polygon.holes])))
        for child in self.children:
            if child is not None:
                fd.write("n{} -> n{};\n".format(id(self), id(child)))
                child.save_dot(fd)
    #TODO
    def traversal(self):
        pass

    """call normalize method on each polygon of the tree
    this is a prerequisite for translated polygon identifications
    """
    def normalize_polygons(self):
        new_polygon = self.holed_polygon
        if new_polygon is not None:
            new_polygon.normalize()
        for c in self.children:
            c.normalize_polygons()

    # assumes holed_polygons in tree are normalized
    def replace_translated_polygons(self, original_polygons):
        new_polygon = self.holed_polygon
        if new_polygon is not None:
            points_number = new_polygon.polygon.points_number()
            same_degree_polygons = original_polygons[points_number]
            for original in same_degree_polygons:
                if new_polygon.is_translated(original):
                    self.holed_polygon = translated_holed_polygon(original, new_polygon)
                    break
            else:
                original_polygons[points_number].append(new_polygon)

        for c in self.children:
            c.replace_translated_polygons(original_polygons)

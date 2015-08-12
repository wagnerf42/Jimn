from jimn.holed_polygon import holed_polygon
from jimn.translated_holed_polygon import translated_holed_polygon
from jimn.displayable import tycat
from jimn.polygontree.tree import tree
import os
import getpass

dot_count = 0


class polygontree(tree):
    def __init__(self, holed_polygon=None):
        self.holed_polygon = holed_polygon
        self.children = []

    def add_child(self, polygon, height, holes):
        new_child = polygontree(holed_polygon(polygon, height, holes))
        self.children.append(new_child)
        return new_child

    def display_depth_first(self):
        carved_polygons = []
        for node in self.depth_first_exploration():
            if node.holed_polygon is not None:
                carved_polygons.append(node.holed_polygon)
                tycat(*carved_polygons)

    def display_breadth_first(self):
        carved_polygons = []
        for node in self.breadth_first_exploration():
            if node.holed_polygon is not None:
                carved_polygons.append(node.holed_polygon)
                tycat(*carved_polygons)

    def tycat(self):
        global dot_count
        user = getpass.getuser()
        directory = "/tmp/{}".format(user)
        if not os.path.exists(directory):
            os.makedirs(directory)
        dot_file = "{}/ptree_{}.dot".format(directory, dot_count)
        svg_file = "{}/ptree_{}.svg".format(directory, dot_count)
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
            if self.holed_polygon is None:
                fd.write("n{} [label=\"None\"];\n".format(
                    id(self))
                )
            elif not self.holed_polygon.holes:
                fd.write("n{} [label=\"{}, h={}\"];\n".format(
                    id(self), str(self.holed_polygon.polygon.label),
                    str(self.holed_polygon.height))
                )
            else:
                fd.write("n{} [label=\"{}, h={}\nholes={}\"];\n".format(
                    id(self), str(self.holed_polygon.polygon.label),
                    str(self.holed_polygon.height),
                    str([h.label for h in self.holed_polygon.holes]))
                )
            for child in self.children:
                fd.write("n{} -> n{};\n".format(id(self), id(child)))

    """call normalize method on each polygon of the tree
    this is a prerequisite for translated polygon identifications
    """
    def normalize_polygons(self):
        for node in self.depth_first_exploration():
            new_polygon = node.holed_polygon
            if new_polygon is not None:
                new_polygon.normalize()

    # assumes holed_polygons in tree are normalized
    # we do everything in one pass
    def replace_translated_polygons(self, original_polygons):
        for node in self.depth_first_exploration():
            new_polygon = self.holed_polygon
            if new_polygon is not None:
                points_number = new_polygon.polygon.points_number()
                same_degree_polygons = original_polygons[points_number]
                for original in same_degree_polygons:
                    if new_polygon.is_translated(original):
                        self.holed_polygon = \
                            translated_holed_polygon(original, new_polygon)
                        break
                else:
                    original_polygons[points_number].append(new_polygon)

    def offset_polygons(self, carving_radius):
        print("TODO")

    def compute_path(self):
        print("TODO")

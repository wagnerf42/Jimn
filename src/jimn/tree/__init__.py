"""
abstract tree class.
to be derived from.
"""
import os
import getpass
from collections import deque
from jimn.point import Point
from jimn.displayable import tycat


class Tree:
    """
    abstract tree class.
    all jimn trees derive from this class.
    """
    dot_count = 0

    def __init__(self, content=None):
        self.content = content
        # we all tree compression by translations
        # apply following list if translations to self
        # to find all real trees
        self.translations = [Point([0, 0])]
        self.children = []

    def add_translation(self, translation_vector):
        """
        mark node (and whole subtree) as duplicated for
        given translation vector.
        """
        self.translations.append(translation_vector)

    def copy_translations(self, other):
        """
        get same translations to apply as other.
        used to keep translations when converting trees
        """
        self.translations = other.translations

    def get_children(self):
        """
        return children array of current node
        """
        return self.children

    def depth_first_exploration(self):
        """
        iterator : depth first exploration.
        use it as : for n in t.depth_first_exploration()
        """
        stack = []
        stack.append(self)
        while stack:
            node = stack.pop()
            yield node
            stack.extend(node.get_children())

    def breadth_first_exploration(self):
        """
        iterator : breadth first exploration.
        use it as : for n in t.breadth_first_exploration()
        """
        unseen_nodes = deque()
        unseen_nodes.append(self)
        while unseen_nodes:
            node = unseen_nodes.popleft()
            yield node
            unseen_nodes.extend(node.get_children())

    def display_depth_first(self):
        """
        iteratively tycat all tree (depth first).
        """
        _display(self.depth_first_exploration())

    def display_breadth_first(self):
        """
        iteratively tycat all tree (breadth first).
        """
        _display(self.breadth_first_exploration())

    def tycat(self):
        """
        svg display of dot file in terminology.
        """
        user = getpass.getuser()
        directory = "/tmp/{}".format(user)
        if not os.path.exists(directory):
            os.makedirs(directory)

        dot_file = "{}/tree_{}.dot".format(directory, self.dot_count)
        svg_file = "{}/tree_{}.svg".format(directory, self.dot_count)
        Tree.dot_count += 1
        dot_fd = open(dot_file, 'w')
        dot_fd.write("digraph g {\n")
        for node in self.depth_first_exploration():
            node.save_dot(dot_fd)
        dot_fd.write("}")
        dot_fd.close()
        os.system("dot -Tsvg {} -o {}".format(dot_file, svg_file))
        os.system("tycat {}".format(svg_file))

    def save_dot(self, dot_file):
        """
        save tree to dot file.
        """
        if self.content is None:
            label = "\"None\""
        else:
            label = "\"" + self.content.get_dot_label()
            if self.translations:
                if len(self.translations) < 10:
                    label += "(" + \
                        ",".join([str(t) for t in self.translations]) + ")"
                else:
                    label += "(...)"
            label += "\""

        dot_file.write("n{} [label={}];\n".format(
            id(self),
            label
        ))

        for child in self.children:
            dot_file.write("n{} -> n{};\n".format(id(self), id(child)))


def _display(iterator):
    displayed_content = []
    for node in iterator:
        if node.content is not None:
            displayed_content.append(node.content)
            tycat(*displayed_content)

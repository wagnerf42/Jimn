"""
abstract tree class.
to be derived from.
"""
import os
import getpass
from collections import deque
from jimn.displayable import tycat


class Tree:
    """
    abstract tree class.
    all jimn trees derive from this class.
    """
    dot_count = 0

    def __init__(self, content=None):
        self.content = content
        self.children = []

    def depth_first_exploration(self):
        """
        iterator : depth first exploration.
        use it as : for n in t.depth_first_exploration()
        """
        stack = []
        stack.append(self)
        while stack:
            node = stack.pop()
            if node is not None:
                yield node
                stack.extend(node.children)

    def breadth_first_exploration(self):
        """
        iterator : breadth first exploration.
        use it as : for n in t.breadth_first_exploration()
        """
        unseen_nodes = deque()
        unseen_nodes.append(self)
        while unseen_nodes:
            node = unseen_nodes.popleft()
            if node is not None:
                yield node
                unseen_nodes.extend(node.children)

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

    def dot_label(self):
        """
        return label used in dot file when saving node self.
        """
        if self.content is None:
            label = "None"
        else:
            label = self.content.get_dot_label()

        return label

    def save_dot(self, dot_file):
        """
        save tree to dot file.
        """
        dot_file.write("n{} [label={}];\n".format(
            id(self),
            "\"" + self.dot_label() + "\""
        ))

        for child in self.children:
            if child is not None:
                dot_file.write("n{} -> n{};\n".format(id(self), id(child)))


def _display(iterator):
    displayed_content = []
    for node in iterator:
        if node.content is not None:
            displayed_content.append(node.content)
            tycat(*displayed_content)

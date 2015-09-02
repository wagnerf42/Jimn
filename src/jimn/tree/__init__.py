from jimn.displayable import tycat
from collections import deque
import os
import getpass

"""
abstract tree class.
to be derived from
"""

dot_count = 0


class tree:

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
        d = deque()
        d.append(self)
        while d:
            node = d.popleft()
            yield node
            d.extend(node.get_children())

    def display_depth_first(self):
        _display(self.depth_first_exploration())

    def display_breadth_first(self):
        _display(self.breadth_first_exploration())

    def tycat(self):
        global dot_count
        user = getpass.getuser()
        directory = "/tmp/{}".format(user)
        if not os.path.exists(directory):
            os.makedirs(directory)
        dot_file = "{}/tree_{}.dot".format(directory, dot_count)
        svg_file = "{}/tree_{}.svg".format(directory, dot_count)
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
            if self.content is None:
                label = "\"None\""
            else:
                label = self.content.get_dot_label()

            fd.write("n{} [label={}];\n".format(
                id(self),
                label
            ))

            for child in self.children:
                fd.write("n{} -> n{};\n".format(id(self), id(child)))


def _display(iterator):
    displayed_content = []
    for node in iterator:
        if node.content is not None:
            displayed_content.append(node.content)
            tycat(*displayed_content)

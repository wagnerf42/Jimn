"""
providing treap class.
"""

from random import random
from jimn.tree import Tree


class Treap(Tree):
    """
    self-balancing BST.
    """
    def __init__(self, content, father=None, priority=None):
        super().__init__(content)
        self.children = [None, None]
        self.father = father
        if priority is not None:
            self.priority = priority
        else:
            self.priority = random()

    def add(self, content):
        """
        add given content in tree.
        """
        direction = (self.content < content)
        if self.children[direction]:
            self.children[direction].add(content)
        else:
            new_child = Treap(content, father=self)
            self.children[direction] = new_child
            new_child.balance()

    def remove(self):
        """
        remove ourselves from tree.
        """
        # first easy cases : one or zero children
        father = self.father
        if self.children[True] is None:
            father.set_child(father.direction_to(self), self.children[False])
        elif self.children[False] is None:
            father.set_child(father.direction_to(self), self.children[True])
        else:
            # more complex case : find leftmost node in right subtree
            extremum = self.children[True].find_extreme_node(False)
            extremum.exchange_content_with(self)
            extremum.remove()

    def find(self, content):
        """
        search for node with content equal to given one.
        pre-requisite: we contain given content.
        """
        node = self
        while node.content != content:
            direction = node.content < content
            node = node.children[direction]

        return node

    def find_extreme_node(self, direction):
        """
        return node in subtree the most to the given direction.
        """
        node = self
        while node.children[direction] is not None:
            node = node.children[direction]
        return node

    def exchange_content_with(self, other):
        """
        exchange contents.
        """
        self.content, other.content = other.content, self.content

    def direction_to(self, target_child_node):
        """
        which way to target child node ?
        pre-requisite : given node is one of our children.
        """
        return id(self.children[True]) == id(target_child_node)

    def balance(self):
        """
        use priorities to balance tree starting from given node
        and going back to root node.
        """
        node = self
        while node.priority > node.father.priority:
            node.rotate_upwards()

    def set_child(self, direction, child):
        """
        set given node as wanted child.
        """
        self.children[direction] = child
        if child is not None:
            child.father = self

    def rotate_upwards(self):
        """
        push node upwards and father downwards in opposite direction.
        """
        father = self.father
        direction = father.direction_to(self)
        grandfather = father.father
        grandfather_direction = grandfather.direction_to(father)
        # first, replace father for grandfather
        grandfather.set_child(grandfather_direction, self)
        # now exchange roles with father
        reversed_direction = not direction
        father.set_child(direction, self.children[reversed_direction])
        self.set_child(reversed_direction, father)

    def dot_label(self):
        """
        label of given node for dot file.
        """
        return str(self.content) + " / " + str(self.priority)

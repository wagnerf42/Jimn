"""
providing treap class.
"""

from random import random
from jimn.tree import Tree

MAX_PRIORITY = 2


class Treap(Tree):
    """
    self-balancing BST.
    """
    # pylint: disable=protected-access
    def __init__(self, content, root_node=False):
        super().__init__(content)
        self.children = [None, None]
        self.father = None  # for now
        if root_node:
            self.priority = MAX_PRIORITY
        else:
            self.priority = random()

    def add(self, content):
        """
        add given content in tree.
        """
        direction = (self.content < content)
        node = self
        while node.children[direction]:
            node = node.children[direction]
            direction = (node.content < content)

        new_child = Treap(content)
        node._set_child(direction, new_child)
        new_child._balance()

    def remove(self):
        """
        remove ourselves from tree.
        """
        # first easy cases : one or zero children
        father = self.father
        if self.children[True] is None:
            father._set_child(father._direction_to(self), self.children[False])
        elif self.children[False] is None:
            father._set_child(father._direction_to(self), self.children[True])
        else:
            # more complex case : find leftmost node in right subtree
            extremum = self.children[True]._find_extreme_node(False)
            extremum._exchange_content_with(self)
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

    def neighbours(self):
        """
        returns the (up to two) nodes with nearest values
        """
        nodes = []
        for direction in (False, True):
            neighbour = self._nearest_node(direction)
            if neighbour:
                nodes.append(neighbour)

        return nodes

    def dot_label(self):
        """
        label of given node for dot file.
        """
        return str(self.content) + " / " + str(self.priority)

    def _is_sentinel(self):
        """
        are we dummy root node ?
        """
        return self.priority == MAX_PRIORITY

    def _nearest_node(self, direction):
        """
        return node with content value nearest (and smaller / bigger
        depending on direction)
        """
        if self.children[direction] is not None:
            return self.children[direction]._find_extreme_node(not direction)
        else:
            # we need to find in ancestors
            old = self
            older = self.father
            reversed_direction = not direction
            while not older._is_sentinel():
                if older._direction_to(old) == reversed_direction:
                    return older
                old = older
                older = older.father

            return None

    def _find_extreme_node(self, direction):
        """
        return node in subtree the most to the given direction.
        """
        node = self
        while node.children[direction] is not None:
            node = node.children[direction]
        return node

    def _exchange_content_with(self, other):
        """
        exchange contents.
        BE VERY CAREFUL. EXCHANGING CONTENT MIGHT INVALIDATE EXTERNAL POINTERS.
        """
        self.content, other.content = other.content, self.content

    def _direction_to(self, target_child_node):
        """
        which way to target child node ?
        pre-requisite : given node is one of our children.
        """
        return id(self.children[True]) == id(target_child_node)

    def _balance(self):
        """
        use priorities to balance tree starting from given node
        and going back to root node.
        """
        node = self
        while node.priority > node.father.priority:
            node._rotate_upwards()

    def _set_child(self, direction, child):
        """
        set given node as wanted child.
        """
        self.children[direction] = child
        if child is not None:
            child.father = self

    def _rotate_upwards(self):
        """
        push node upwards and father downwards in opposite direction.
        """
        father = self.father
        direction = father._direction_to(self)
        grandfather = father.father
        grandfather_direction = grandfather._direction_to(father)
        # first, replace father for grandfather
        grandfather._set_child(grandfather_direction, self)
        # now exchange roles with father
        reversed_direction = not direction
        father._set_child(direction, self.children[reversed_direction])
        self._set_child(reversed_direction, father)

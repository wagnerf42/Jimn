"""
providing treap class.
"""

from random import random
from jimn.tree import Tree
from jimn.displayable import tycat, Displayer

MAX_PRIORITY = 2


class DummyComparer:
    """
    comparer class returning what we get.
    (dummy function for comparing types not requiring keys).
    """
    # pylint: disable = too-few-public-methods
    def __init__(self):
        pass

    def key(self, thing):
        # pylint: disable = no-self-use
        """
        return what is given.
        """
        return thing

class ConflictingKeys(Exception):
    """
    raised when inserting an element which key is already in tree.
    """
    def __init__(self, existing_node, new_content):
        super().__init__("conflict")
        self.existing_node = existing_node
        self.new_content = new_content

class Treap(Tree):
    """
    self-balancing BST.

    objects entered in tree need to be compared.
    by default we will use objects comparison operator.

    there is however an other way :
    you can provide a comparer object with a "key" method to the bst
    using "set_comparer".
    we will then compare objects by calling the key method from the
    comparer on each of them and compare obtained keys.
    """
    # pylint: disable=protected-access
    comparer = DummyComparer()  # by default we just compare objects

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
        return new node.
        """
        content_key = self.comparer.key(content)
        node_key = self.comparer.key(self.content)
        if content_key == node_key:
            raise ConflictingKeys(self, content)
        direction = node_key < content_key
        node = self
        while node.children[direction] is not None:
            node = node.children[direction]
            node_key = self.comparer.key(node.content)
            if content_key == node_key:
                raise ConflictingKeys(node, content)
            direction = self.comparer.key(node.content) < content_key

        new_child = Treap(content)
        node._set_child(direction, new_child)
        return new_child._balance()

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
            self._exchange_with(extremum)
            self.remove()

        # for more security disconnect pointers
        self.father = None
        self.children = [None, None]

    def find(self, content):
        """
        search for node with content equal to given one.
        pre-requisite: we contain given content.
        """
        node = self
        content_key = self.comparer.key(content)
        while node.content != content:
            direction = self.comparer.key(node.content) < content_key
            node = node.children[direction]

        return node

    def find_object(self, searched_object):
        """
        search for node with content of same id as given object.
        faster comparisons than calling equality operator.
        pre-requisite: we contain it.
        """
        node = self
        object_key = self.comparer.key(searched_object)
        while id(node.content) != id(searched_object):
            direction = self.comparer.key(node.content) < object_key
            node = node.children[direction]

        assert node.content == searched_object
        return node

    def set_comparer(self, comparer):
        """
        set comparer object for comparing nodes content.
        object needs to provide a "key" method returning a comparison key
        for an object.
        """
        self.comparer = comparer

    def neighbours(self):
        """
        return the (up to two) nodes with nearest values.
        """
        nodes = []
        for direction in (False, True):
            neighbour = self.nearest_node(direction)
            if neighbour:
                nodes.append(neighbour)

        return nodes

    def nearest_node(self, direction):
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

    def greater_nodes(self):
        """
        iterate on all nodes greater than given one.
        """
        if self.children[True] is not None:
            for node in self.children[True].infix_exploration():
                yield node

        current_node = self
        while not current_node._is_sentinel():
            father = current_node.father
            if not father._direction_to(current_node):
                yield father
                if father.children[True] is not None:
                    for node in father.children[True].infix_exploration():
                        yield node
            current_node = father

    def dot_label(self):
        """
        label of given node for dot file.
        """
        # pylint: disable=no-member
        if self._is_sentinel():
            return str(self.content) + " / " + str(self.priority)
        else:
            return str(self.content) + " / " + \
                str(self.father._direction_to(self)) + \
                "\",style=filled,color=\"" + self.color

    def ordered_contents(self):
        """
        returns list of all content (ordered).
        pre-requisite: called on root node.
        """
        child = self.children[False]
        if child is None:
            return []
        return [n.content for n in child.infix_exploration()]

    def infix_exploration(self):
        """
        depth first infix exploration.
        """
        seen_nodes = []
        current_node = self
        while current_node is not None or seen_nodes:
            if current_node is not None:
                seen_nodes.append(current_node)
                current_node = current_node.children[False]
            else:
                current_node = seen_nodes.pop()
                yield current_node
                current_node = current_node.children[True]

    def debug_find(self, searched_object):
        """
        find where it went wrong in find algorithm.
        pre-requisite: self is root
        """
        # pylint: disable=no-member
        assert self.children[True] is None
        assert self._is_sentinel(), "debug finds on root node only."

        searched_node = self._raw_search(searched_object)
        if searched_node is None:
            print("searched object", searched_object, "is not in tree")
            return

        self._compute_nodes_colors()
        ancestors = searched_node._ancestors()

        # now replay search, looking for wrong comparison
        searched_key = self.comparer.key(searched_object)
        node = self
        expected_node = ancestors.pop()
        while id(node) == id(expected_node):
            node_key = self.comparer.key(node.content)
            direction = node_key < searched_key
            next_node = node.children[direction]
            next_expected_node = ancestors.pop()

            if next_node is None or id(next_node) != id(next_expected_node):
                print("wrong comparison at node", node.dot_label())
                if node._is_sentinel():
                    print("wrong node is root node")
                else:
                    print("wrong node is colored as", node.color)

                print("searched object's key:", [str(k) for k in searched_key])
                print("node's key:", [str(k) for k in node_key])
                print("we should have gone", not direction)
                if direction:
                    print("searched key should have been smaller")
                else:
                    print("searched key should have been bigger")

                small_child = node.children[False]
                if small_child is not None:
                    searched = small_child._raw_search(searched_object)
                    if searched is not None:
                        print("we found it in smaller children")
                        tycat(searched_object, node.content)
                        return

                big_child = node.children[True]
                if big_child is not None:
                    searched = big_child._raw_search(searched_object)
                    if searched is not None:
                        print("we found it in bigger children")
                        tycat(searched_object, node.content)

                return

            node = next_node
            expected_node = next_expected_node

    def tycat(self):
        """
        colored graph.
        """
        self._compute_nodes_colors()
        super().tycat()

    def _is_sentinel(self):
        """
        are we dummy root node ?
        """
        return self.priority == MAX_PRIORITY

    def _find_extreme_node(self, direction):
        """
        return node in subtree the most to the given direction.
        """
        node = self
        while node.children[direction] is not None:
            node = node.children[direction]
        return node

    def _exchange_with(self, other):
        """
        exchange nodes in tree.
        pre-requisite: self is ancestor of other.
        """
        father = self.father
        other_father = other.father
        children = list(self.children)
        other_children = list(other.children)

        direction = father._direction_to(self)
        other_direction = other_father._direction_to(other)
        father._set_child(direction, other)

        for direction in (False, True):
            self._set_child(direction, other_children[direction])

        if id(other_father) == id(self):
            # special case : exchanging with direct child
            other._set_child(other_direction, self)
            other._set_child(not other_direction,
                             children[not other_direction])
        else:
            other_father._set_child(other_direction, self)
            for direction in (False, True):
                other._set_child(direction, children[direction])

        # exchange priorities
        # TODO: rebalance instead ????
        self.priority, other.priority = other.priority, self.priority

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
        return starting node at its new position.
        """
        node = self
        while node.priority > node.father.priority:
            node._rotate_upwards()
        return node

    def _set_child(self, direction, child):
        """
        set given node as wanted child.
        """
        self.children[direction] = child
        if child is not None:
            child.father = self
            if __debug__:
                if self._is_sentinel() and direction:
                    raise Exception("adding right child to root node")

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

    def _compute_nodes_colors(self):
        """
        add color attribute to each node.
        """
        if self.children[False] is None:
            return
        count = 3
        for node in self.children[False].infix_exploration():
            node.color = Displayer.svg_colors[
                count % len(Displayer.svg_colors)]
            count += 1

    def _raw_search(self, searched_object):
        """
        steps through ALL nodes, looking for object.
        used in debugging.
        """
        for node in self.depth_first_exploration():
            if id(node.content) == id(searched_object):
                return node
        else:
            return

    def _ancestors(self):
        """
        return list of all ancestors of node
        (including node and starting from it).
        """
        node = self
        ancestors = []
        while node is not None:
            ancestors.append(node)
            node = node.father

        return ancestors

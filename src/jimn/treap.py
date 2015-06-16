# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
import os
import getpass
import random


"""
this module implement treaps storing typed objects
we maintain on each node of the tree a hash table storing
number of nodes of each types in the corresponding subtree

stored objects need to provide
 - comparison operator (lt)
 - equality operator (eq)
 - a 'get_type' method returning type of the object

'remove' method might invalidate external pointers
"""

dot_count = 0


class treap:

    # public methods
    def treap_root(content):
        self = treap(content)
        self.priority = 1.1  # greater than any other
        return self

    def find(self, content):
        current_node = self
        while current_node is not None:
            if current_node.content == content:
                return current_node
            direction = current_node.direction_for(content)
            current_node = current_node.children[direction]
        return None

    def add(self, content):
        father = self
        direction = father.direction_for(content)
        while father.children[direction] is not None:
            father = father.children[direction]
            direction = father.direction_for(content)
        new_node = treap(content)
        father.set_child_node(direction, new_node)
        new_node.modify_count(1, content.get_type())
        new_node = new_node.balance()
        return new_node

    # precondition: never called on root
    # be wary that since remove uses exchanges by values
    # using this function MAY INVALIDATE ANY EXTERNAL non-root POINTER
    # TODO : rebalance on removal ?
    def remove(self):
        children_number = self.children_number()

        # handle each different possible case
        if children_number == 0:
            self.father.modify_count(-1, self.content.get_type())
            incoming_direction = self.father.direction_for_node(self)
            self.father.children[incoming_direction] = None
        elif children_number == 1:
            self.father.modify_count(-1, self.content.get_type())
            incoming_direction = self.father.direction_for_node(self)
            child = self.unique_child()
            self.father.set_child_node(incoming_direction, child)
        else:
            target = self.children[0].find_extreme_child(1)
            self.exchange(target)
            target.remove()

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

    def count_inferior_nodes(self, key):
        """return the number of nodes of type 'key' which contents are less (strictly) than self.content"""
        count = 0
        if self.children[0] is not None:
            if key in self.children[0].types:
                count = self.children[0].types[key]
        current_node = self
        # move up the tree
        # any 'small' ancestor means we have some more smaller nodes
        while current_node.father is not None:
            incoming_direction = current_node.father.direction_for_node(current_node)
            father_count = 0
            if key in current_node.father.types:
                father_count = current_node.father.types[key]
            child_count = 0
            if key in current_node.types:
                child_count = current_node.types[key]
            if incoming_direction == 1:
                count += father_count - child_count
            current_node = current_node.father
        return count

    # internal methods ('private')
    def set_child_node(self, direction, child):
        self.children[direction] = child
        if child is not None:
            child.father = self

    def exchange(self, target):
        target.modify_count(-1, target.content.get_type())
        target.modify_count(1, self.content.get_type())
        self.modify_count(-1, self.content.get_type())
        self.modify_count(1, target.content.get_type())
        (self.content, target.content) = (target.content, self.content)

    def find_extreme_child(self, direction):
        node = self
        while node.children[direction] is not None:
            node = node.children[direction]
        return node

    def unique_child(self):
        count = 0
        for child in self.children:
            if child is not None:
                count = count + 1
                unique = child
        if count == 1:
            return unique
        else:
            raise NameError('unique child called on node with several children')

    def children_number(self):
        count = 0
        for child in self.children:
            if child is not None:
                count = count + 1
        return count

    def direction_for_node(self, target):
        target_id = id(target)
        for direction in (0, 1):
            child = self.children[direction]
            if id(child) == target_id:
                return direction
        raise NameError('target id is not here')

    def direction_for(self, content):
        if self.content == content:
            raise NameError('looking for self')
        if self.content < content:
            return 1
        else:
            return 0

    def __str__(self):
        strings = []
        for child in self.children:
            if child is not None:
                strings.append(str(child.content))
            else:
                strings.append('none')
        return "[{} : ({}, {})]".format(str(self.content), *strings)

    def save_dot(self, fd):
        fd.write("n{} [label=\"{} ({})\"];\n".format(id(self), str(self.content), self.types))
        if self.father is not None:
            fd.write("n{} -> n{};\n".format(id(self.father), id(self)))
        for child in self.children:
            if child is not None:
                fd.write("n{} -> n{};\n".format(id(child), id(self)))
                child.save_dot(fd)

    def __init__(self, content, father=None, left_child=None, right_child=None):
        self.content = content
        self.father = father
        self.priority = random.random()
        self.children = [left_child, right_child]
        # count how many nodes of each type in subtree
        self.types = {}

    def modify_count(self, amount, key):
        """add 'amount' to counters related to 'key' from here to root"""
        current_node = self
        while current_node is not None:
            if key not in current_node.types:
                current_node.types[key] = 0
            current_node.types[key] += amount
            current_node = current_node.father

    def merge_counts(self, other):
        """add all type counters from other into self"""
        if other is None:
            return
        for key in other.types:
            count = other.types[key]
            if key not in self.types:
                self.types[key] = 0
            self.types[key] += count

    def balance(self):
        current_node = self
        while current_node.priority > current_node.father.priority:
            current_node.rotate_upwards()
        return current_node

    def rotate_upwards(self):
        father = self.father
        grand_father = father.father
        incoming_direction = father.direction_for_node(self)
        father_incoming_direction = grand_father.direction_for_node(father)
        grand_father.set_child_node(father_incoming_direction, self)
        moved_child = self.children[1 - incoming_direction]
        self.set_child_node(1 - incoming_direction, father)
        father.set_child_node(incoming_direction, moved_child)
        # modify counting of nodes in subtree
        self.types = father.types
        father.types = {}
        father.types[father.content.get_type()] = 1
        father.merge_counts(self.children[incoming_direction])
        father.merge_counts(father.children[1-incoming_direction])
        father.merge_counts(moved_child)

# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
import os
import getpass
import random

dot_count = 0


class treap:

    def __init__(self, content, father=None, left_child=None, right_child=None):
        self.content = content
        self.father = father
        self.priority = random.random()
        self.children = [left_child, right_child]

    def treap_root(content):
        self = treap(content)
        self.priority = 1.1 # greater than any other
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
        return new_node.balance()

    # precondition: never called on root
    # be wary that since remove uses exchanges by values
    # using this function MAY INVALIDATE ANY EXTERNAL non-root POINTER
    # TODO : rebalance on removal ?
    def remove(self):
        children_number = self.children_number()
        if children_number == 0:
            incoming_direction = self.father.direction_for_node(self)
            self.father.children[incoming_direction] = None
        elif children_number == 1:
            incoming_direction = self.father.direction_for_node(self)
            child = self.unique_child()
            self.father.set_child_node(incoming_direction, child)
        else:
            target = self.children[0].find_extreme_child(1)
            self.exchange(target)
            target.remove()

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

    def set_child_node(self, direction, child):
        self.children[direction] = child
        if child is not None:
            child.father = self

    def exchange(self, target):
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
        fd.write("n{} [label={}];\n".format(id(self), str(self.content)))
        if self.father is not None:
            fd.write("n{} -> n{};\n".format(id(self.father), id(self)))
        for child in self.children:
            if child is not None:
                child.save_dot(fd)

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

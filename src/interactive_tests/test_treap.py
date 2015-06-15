#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.treap import treap


class test_obj:
    def __init__(self, value, key):
        self.value = value
        self.key = key

    def __lt__(self, other):
        return self.value < other.value

    def __eq__(self, other):
        return self.value == other.value

    def get_type(self):
        return self.key

    def __str__(self):
        return "{}/{}".format(self.value, self.key)

t = treap.treap_root(test_obj(0, -1))

for i in (range(1, 5)):
    print("adding type 0 {}\n".format(i))
    t.add(test_obj(i, 0))
    t.tycat()

for i in (range(6, 10)):
    print("adding type 1 {}\n".format(i))
    t.add(test_obj(i, 1))
    t.tycat()

for i in (7, 4, 3):
    print("removing {}\n".format(i))
    node = t.find(test_obj(i, 0))
    node.remove()
    t.tycat()

#!/usr/bin/env python3

from time import clock
from random import random, seed, shuffle
from jimn.tree.treap import Treap

root = Treap(-1, root_node=True)
seconds = clock()
print(seconds)
seed(seconds)

print("creation")
values = []
for _ in range(10):
    new_value = random()
    values.append(new_value)
    print("adding:", new_value)
    root.add(new_value)
    root.tycat()

print("neighbours")
for value in sorted(values):
    neighbours = [n.content for n in root.find(value).neighbours()]
    print(value, "is neighboured by", [str(v) for v in neighbours])

print("destruction")
shuffle(values)
for value in values:
    print("removing:", value)
    node = root.find(value)
    node.remove()
    root.tycat()

#!/usr/bin/env python3

from time import clock
from random import random, seed, shuffle
from jimn.tree.treap import Treap

root = Treap(-1, priority=1000)
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

print("destruction")
for value in values:
    print("removing:", value)
    node = root.find(value)
    node.remove()
    root.tycat()

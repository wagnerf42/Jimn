#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.treap import treap

t = treap.treap_root(0)

for i in (range(1,10)):
    print("adding {}\n".format(i))
    t.add(i)
    t.tycat()

for i in (7,5,3):
    print("removing {}\n".format(i))
    node = t.find(i)
    node.remove()
    t.tycat()

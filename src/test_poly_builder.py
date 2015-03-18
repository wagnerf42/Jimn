#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.stl import stl
from jimn.stl import projection2d
from jimn.displayable import tycat
from jimn.poly_builder import *
from jimn.point import point

scene = stl('../test_files/Box0.stl')
# tycat(projection2d(scene.horizontal_intersection(0.5)))
lseg = projection2d(scene.horizontal_intersection(0.5))
dico = hash_points(lseg)
print(dico)
sorted_dico = sort_dico(dico)
print(angle(point(0,0), point(1,1)))

p1 = point(0,0)
p2 = point(1,1)
p3 = point(1,-1)
dico_test = {p1 : [p2, p3]}
for p in (dico_test[p1]):
    print(p)
sort_dico(dico_test)
for p in (dico_test[p1]):
    print(p)
# print(sorted_dico)

s1 = segment(p1, p2)
s2 = segment(p2, p3)
lseg = [s2, s1]
for seg in lseg:
    print(seg)
sorted_lseg = sort_lseg(lseg)
for seg in sorted_lseg:
    print(seg)



scene2 = stl('../test_files/cordoba.stl')
# tycat(projection2d(scene2.horizontal_intersection(0.8)))

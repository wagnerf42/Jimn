#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.stl import stl
from jimn.point import point
from jimn.segment import segment
from jimn.stl import projection2d
from jimn.displayable import tycat
from jimn.poly_builder import build_polygons
from jimn.poly_builder import hash_points
from jimn.poly_builder import print_neighbors

# scene = stl('../test_files/Box0.stl')
# tycat(projection2d(scene.horizontal_intersection(0.5)))
# lseg = projection2d(scene.horizontal_intersection(0.5))

p1 = point([0, 0])
p2 = point([1, 1])
p3 = point([1, -1])
#dico_test = {p1: [p2, p3]}
#for p in (dico_test[p1]):
#    print(p)
#sort_segments_of_points(dico_test)
#for p in (dico_test[p1]):
#    print(p)
#
s1 = segment([p1, p2])
s2 = segment([p2, p3])
s3 = segment([p1, p3])
lseg = [s2, s1, s3]
#for seg in lseg:
#    print(seg)
#sorted_lseg = sort_lseg(lseg)
#for seg in sorted_lseg:
#    print(seg)



scene2 = stl('../test_files/cordoba_2.stl')
lseg = projection2d(scene2.horizontal_intersection(1))



tycat(lseg)
#sorted_lseg = sort_lseg(lseg)

#dico = hash_points(lseg)
#print_neighbors(dico, lseg)
#sort_segments_of_points(dico)
#lpoly = build_lpoly(sorted_lseg, dico, lseg)
#poly = lpoly[0]
#for p in poly:
#    tycat(p)
polygons = build_polygons(lseg)
tycat(*polygons)


# scene2 = stl('../test_files/cordoba.stl')
# tycat(projection2d(scene2.horizontal_intersection(0.8)))

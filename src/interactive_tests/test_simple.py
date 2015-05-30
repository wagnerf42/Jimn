#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.point import point
from jimn.segment import segment
from jimn.displayable import tycat
from jimn.poly_builder import build_polygons
from jimn.poly_builder import sort_segments
#from jimn.poly_builder import hash_points
#from jimn.poly_builder import print_neighbors

a = point([0, 0])
b = point([-1, 0])
c = point([0, -1])
d = point([1, 0])
e = point([0, 1])

ab = segment([a, b])
bc = segment([b, c])
ac = segment([a, c])
cd = segment([c, d])
ad = segment([a, d])
de = segment([d, e])
ea = segment([e, a])

lseg = [ab, bc, ac, cd, ad, de, ea]
tycat(lseg)
polygons = build_polygons(lseg)
tycat(*polygons)



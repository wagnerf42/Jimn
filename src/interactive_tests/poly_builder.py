#!/usr/bin/env python3

from jimn.stl import Stl
from jimn.displayable import tycat
from jimn.pocket.builder import build_polygons


scene2 = Stl('../test_files/cordoba_2.stl')
lseg = scene2.horizontal_intersection(1)
tycat(lseg)

polygons = build_polygons(lseg)
tycat(*polygons)

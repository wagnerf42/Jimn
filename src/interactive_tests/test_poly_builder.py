#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.stl import stl
from jimn.point import point
from jimn.segment import segment
from jimn.stl import projection2d
from jimn.displayable import tycat
from jimn.poly_builder import polygonbuilder


scene2 = stl('../test_files/cordoba_2.stl')
lseg = projection2d(scene2.horizontal_intersection(1))
tycat(lseg)

builder = polygonbuilder(lseg)
polygons = builder.build_polygons()
tycat(*polygons)

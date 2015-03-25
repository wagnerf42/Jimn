#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.stl import stl
from jimn.stl import projection2d
from jimn.displayable import tycat

scene = stl('../test_files/Box0.stl')
tycat(projection2d(scene.horizontal_intersection(0.5)))

scene2 = stl('../test_files/cordoba.stl')
tycat(projection2d(scene2.horizontal_intersection(1)))

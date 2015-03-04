#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.stl import stl
from jimn.displayable import tycat

scene = stl('../test_files/Box0.stl')
tycat(scene.project_half_space_intersection(0.5))

scene2 = stl('../test_files/cordoba.stl')
tycat(scene2.project_half_space_intersection(0.8))

#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.stl import stl
from jimn.displayable import tycat

scene2 = stl('../test_files/cordoba_2.stl')
tycat(scene2.horizontal_intersection(1))

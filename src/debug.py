#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.stl import stl
from jimn.stl import projection2d
from jimn.displayable import tycat


model = stl('../test_files/cordoba.stl')
sl = projection2d(model.horizontal_intersection(0.9999999761581421))
tycat(sl)
#0.299999976158142

#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.stl import stl
from jimn.stl import projection2d
from jimn.displayable import tycat
from jimn.segment_merger import segment_merger


model = stl('../test_files/cordoba.stl')
sl = projection2d(model.horizontal_intersection(0.9999999761581421))
tycat(sl)
merger = segment_merger(sl)
tycat(merger.merge())

sl2 = projection2d(model.horizontal_intersection(0.95))
tycat(sl2)
merger = segment_merger(sl2)
tycat(merger.merge())

sl3 = projection2d(model.horizontal_intersection(0.90))
tycat(sl3)
merger = segment_merger(sl3)
tycat(merger.merge())
#0.299999976158142

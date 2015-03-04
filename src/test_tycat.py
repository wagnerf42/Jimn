#!/usr/bin/env python3
#vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.point import *
from jimn.segment import *
from jimn.displayable import *

p2 = point(1.0, 2.0)
print(str(p2))
q2 = point(3.0, 4.0)
print(str(q2))
r2 = point(5.0, 6.0)
print(str(r2))
s = segment(q2,r2)

tycat(p2, [q2, r2, s])

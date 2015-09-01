#!/usr/bin/python3

from jimn.point import point
from jimn.segment import segment
from jimn.arc import arc
from jimn.pocket import pocket
from jimn.pocket.elementary_paths import pocket_intersect_paths
from jimn.displayable import tycat, tycat_set_svg_dimensions
import random
import sys

tycat_set_svg_dimensions(800, 600)

if len(sys.argv) > 1:
    size = int(sys.argv[1])
else:
    size = 10
if len(sys.argv) > 2:
    random.seed(float(sys.argv[2]))


paths = []
for i in range(size):
    if random.randint(0, 1) == 0:
        p = segment([
            point([
                random.random(), random.random()
            ]),
            point([
                random.random(), random.random()
            ]),
        ])
    else:
        end1 = point([
            random.random(), random.random()
        ])
        end2 = point([
            random.random(), random.random()
        ])
        d = end1.distance_to(end2)
        p = arc(random.uniform(d/2, 3*d), [end1, end2])

    paths.append(p)

big = pocket(paths)
tycat(big)
small = pocket_intersect_paths(big, paths)
tycat(*small.get_content())

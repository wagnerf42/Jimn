#!/usr/bin/python3

from jimn.point import point
from jimn.segment import segment
from jimn.arc import arc
from jimn.ghost import ghost
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
        p = arc(
            random.random(),
            [
                point([
                    random.random(), random.random()
                ]),
                point([
                    random.random(), random.random()
                ]),
            ]
        )

    paths.append(p)

big = ghost(paths)
tycat(*big.get_content())
small = big.compute_elementary_paths(paths)
tycat(*small.get_content())

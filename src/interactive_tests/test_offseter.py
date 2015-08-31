#!/usr/bin/env python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.displayable import tycat
from jimn.algorithms.offseter import offset_holed_polygon

polygons = [
    polygon([
        point([4, 0]),
        point([3, -2]),
        point([2, -0.6]),
        point([1, -2]),
        point([0, 0]),
    ]),

    polygon([
        point([0, 0]),
        point([3, 0]),
        point([3, -3]),
        point([0, -3]),
    ]),

    polygon([
        point([0, 0]),
        point([-2, 1]),
        point([-0.6, 2]),
        point([-2, 3]),
        point([0, 4]),
    ]),

    polygon([
        point([0, 0]),
        point([0.5, 0]),
        point([0.5, -2]),
        point([1.5, -5]),
        point([-0.5, -5]),
        point([0, -2]),
    ])
]

descriptions = ["polygon cut in pieces", "basic", "vertical cut in pieces",
                "overlapping segments"]

for radius in (0.2, 0.5):
    for index, p in enumerate(polygons):
        print(descriptions[index], radius)
        pockets = offset_holed_polygon(radius, p)
        tycat(p, *pockets)

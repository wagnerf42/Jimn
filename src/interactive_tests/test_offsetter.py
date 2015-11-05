#!/usr/bin/env python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.displayable import tycat
from jimn.algorithms.offsetter import offset_holed_polygon

# special cases
descriptions = [
    "square, carved with radius small enough",
    "square, carved with radius too big",
    "foo",
    "bar"
]
special = [
    polygon.square(0, 0, 3).orient(False),
    polygon.square(0, 0, 0.5).orient(False),

    polygon([
        point([0.5996499807015012, 4.0]),
        point([0.65139999270438, 4.125000018626526]),
        point([0.776400011330862, 4.073250066228293]),
        point([0.806750015504649, 4.0]),
        point([0.776400011330862, 3.926750052981086]),
        point([0.65139999270438, 3.8749999813734735])
    ]).orient(False),

    polygon([
        point([-1.0864993963549738, 0.9135006036450262]),
        point([-1.050676115570425, 1.0]),
        point([-1.0864993963549738, 1.0864993963549738]),
        point([-1.0, 1.050676115570425]),
        point([-0.9135006036450262, 1.0864993963549738]),
        point([-0.9493238248249302, 1.0]),
        point([-0.9135006036450262, 0.9135006036450262]),
        point([-1.0, 0.9493238248249302])
    ]).orient(False)
]

sizes = (1, 1, 0.5, 0.05)
for i, poly in enumerate(special):
    print(descriptions[i])
    tycat(list(poly.segments()))
    pockets = offset_holed_polygon(sizes[i], poly)
    tycat(poly, *pockets)

# standard cases

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

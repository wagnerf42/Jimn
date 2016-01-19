#!/usr/bin/env python3

from jimn.point import Point
from jimn.polygon import Polygon
from jimn.displayable import tycat
from jimn.algorithms.offsetter import offset_holed_polygon
from jimn.utils.debug import add_module_to_debug


print("polygon with hole reaching border")

outer = Polygon.square(0, 0, 10).orient(False)
inner = Polygon.square(1, 1, 3).orient(True)

tycat(outer, inner)
pockets = offset_holed_polygon(2, outer, inner)
tycat(outer, inner, *pockets)

print("polygon with thin part")

outer = Polygon([
    Point([0, 0]),
    Point([0, 1]),
    Point([3, 1]),
    Point([3, 10]),
    Point([10, 10]),
    Point([10, 0]),
])

tycat(outer)
pockets = offset_holed_polygon(2, outer)
tycat(outer, *pockets)

# special cases
descriptions = [
    "square, carved with radius small enough",
    "square, carved with radius too big",
    "foo (radius too big)",
    "bar (radius barely small enough)"
]
special = [
    Polygon.square(0, 0, 3).orient(False),
    Polygon.square(0, 0, 0.5).orient(False),

    Polygon([
        Point([0.5996499807015012, 4.0]),
        Point([0.65139999270438, 4.125000018626526]),
        Point([0.776400011330862, 4.073250066228293]),
        Point([0.806750015504649, 4.0]),
        Point([0.776400011330862, 3.926750052981086]),
        Point([0.65139999270438, 3.8749999813734735])
    ]).orient(False),

    Polygon([
        Point([-1.0864993963549738, 0.9135006036450262]),
        Point([-1.050676115570425, 1.0]),
        Point([-1.0864993963549738, 1.0864993963549738]),
        Point([-1.0, 1.050676115570425]),
        Point([-0.9135006036450262, 1.0864993963549738]),
        Point([-0.9493238248249302, 1.0]),
        Point([-0.9135006036450262, 0.9135006036450262]),
        Point([-1.0, 0.9493238248249302])
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
    Polygon([
        Point([4, 0]),
        Point([3, -2]),
        Point([2, -0.6]),
        Point([1, -2]),
        Point([0, 0]),
    ]),

    Polygon([
        Point([0, 0]),
        Point([3, 0]),
        Point([3, -3]),
        Point([0, -3]),
    ]),

    Polygon([
        Point([0, 0]),
        Point([-2, 1]),
        Point([-0.6, 2]),
        Point([-2, 3]),
        Point([0, 4]),
    ]),

    Polygon([
        Point([0, 0]),
        Point([0.5, 0]),
        Point([0.5, -2]),
        Point([1.5, -5]),
        Point([-0.5, -5]),
        Point([0, -2]),
    ])
]

descriptions = ["polygon cut in pieces", "basic", "vertical cut in pieces",
                "overlapping segments"]

for radius in (0.2, 0.5):
    for index, p in enumerate(polygons):
        print(descriptions[index], radius)
        pockets = offset_holed_polygon(radius, p)
        tycat(p, *pockets)

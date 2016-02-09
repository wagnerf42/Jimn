#!/usr/bin/env python3
from jimn.point import Point
from jimn.segment import Segment
from jimn.arc import Arc
from jimn.pocket import Pocket

outside = Pocket([
    Arc(
    0.1,
    [
        Point([0.6108931327431599, 8.610892234383819]),
        Point([0.7099999875823537, 8.569832184705644])
    ],
    Point([0.6927499771515307, 8.668333134650995]),
    False
),
    Arc(
    0.1,
    [
        Point([0.7099999875823537, 8.569832184705644]),
        Point([0.809106818329299, 8.610892200050886])
    ],
    Point([0.7272499980131766, 8.668333134650995]),
    False
),
    Arc(
    0.1,
    [
        Point([0.809106818329299, 8.610892200050886]),
        Point([0.850167598867855, 8.71])
    ],
    Point([0.7516667163372517, 8.692749604622147]),
    False
),
    Arc(
    0.1,
    [
        Point([0.850167598867855, 8.71]),
        Point([0.8091068183292991, 8.809107799949116])
    ],
    Point([0.7516667163372516, 8.727250395377855]),
    False
),
    Arc(
    0.1,
    [
        Point([0.8091068183292991, 8.809107799949116]),
        Point([0.7099999875823537, 8.850167815294357])
    ],
    Point([0.7272499980131766, 8.751666865349007]),
    False
),
    Arc(
    0.1,
    [
        Point([0.7099999875823537, 8.850167815294357]),
        Point([0.6108931327431599, 8.809107765616183])
    ],
    Point([0.6927499771515307, 8.751666865349007]),
    False
),
    Arc(
    0.1,
    [
        Point([0.6108931327431599, 8.809107765616183]),
        Point([0.5698324011321452, 8.71])
    ],
    Point([0.6683332836627485, 8.727250395377855]),
    False
),
    Arc(
    0.1,
    [
        Point([0.5698324011321452, 8.71]),
        Point([0.6108931327431599, 8.610892234383819])
    ],
    Point([0.6683332836627485, 8.692749604622147]),
    False
)
])

inside = Pocket([
    Arc(
    0.1,
    [
        Point([0.6472195667839512, 8.593235149024423]),
        Point([0.5932357624796265, 8.647217775620348])
    ],
    Point([0.6855833065112172, 8.685583530028849]),
    True
),
    Segment([Point([0.5932357624796265, 8.647217775620348]), Point([0.60040243311994, 8.629967380242494])]),
    Arc(
    0.1,
    [
        Point([0.60040243311994, 8.629967380242494]),
        Point([0.6108931327431599, 8.610892234383819])
    ],
    Point([0.6927499771515307, 8.668333134650995]),
    False
),
    Arc(
    0.1,
    [
        Point([0.6108931327431599, 8.610892234383819]),
        Point([0.6299695439354823, 8.600401223617721])
    ],
    Point([0.6683332836627485, 8.692749604622147]),
    False
),
    Segment([Point([0.6299695439354823, 8.600401223617721]), Point([0.6472195667839512, 8.593235149024423])])
])

if outside.is_included_in(inside):
    print("NOT OK")
else:
    print("OK")

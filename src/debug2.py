#!/usr/bin/env python3
from jimn.point import Point
from jimn.segment import Segment
from jimn.arc import Arc
from jimn.pocket import Pocket
from jimn.displayable import tycat
from jimn.envelope import Envelope

outer_path = Segment(
    [
        Point([5.3, 6.1000000000000005]),
        Point([5, 6.1000000000000005])
    ]
)

inner_pocket = Pocket([
    Arc(
        0.05,
        [
            Point([5.092559893807078, 6.092559893807078]),
            Point([5.092559919557491, 6.127440275423101])
        ],
        Point([5.045700118541718, 6.11000011920929]),
        False
    ),
    Arc(
        0.05,
        [
            Point([5.092559919557491, 6.127440275423101]),
            Point([5.127440249672757, 6.127440249672757])
        ],
        Point([5.11000011920929, 6.174300060272217]),
        False
    ),
    Arc(
        0.05,
        [
            Point([5.127440249672757, 6.127440249672757]),
            Point([5.127440275423101, 6.092559919557491])
        ],
        Point([5.174300060272217, 6.110000119209289]),
        False
    ),
    Arc(
        0.05,
        [
            Point([5.127440275423101, 6.092559919557491]),
            Point([5.092559893807078, 6.092559893807078])
        ],
        Point([5.11000011920929, 6.045700118541718]),
        False
    )
])

tycat(outer_path, inner_pocket)
outer_envelope = Envelope(outer_path, 0.05)
inner_envelope = Envelope(inner_pocket, 0.05)
tycat(outer_envelope, inner_envelope)

points = outer_envelope.junction_points(inner_envelope)
print(points)

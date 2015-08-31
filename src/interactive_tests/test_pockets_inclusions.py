#!/usr/bin/env python

from jimn.point import point
from jimn.displayable import tycat
from jimn.utils.iterators import all_two_elements
from jimn.segment import segment
from jimn.pocket import pocket

# testing inclusion

square = [
    point([0.0, 5.0]),
    point([1.0, 5.0]),
    point([1.0, 6.0]),
    point([0.0, 6.0]),
]

tested_pocket = pocket([
    segment([a, b]) for a, b in all_two_elements(square)
])

including = [
    point([-1.0, 4.0]),
    point([2.0, 4.0]),
    point([2.0, 7.0]),
    point([-1.0, 7.0]),
]

including_pocket = pocket([
    segment([a, b]) for a, b in all_two_elements(including)
])

not_including = [
    point([-2.0, 3.0]),
    point([-2.0, 2.0]),
    point([9.0, 2.0]),
    point([9.0, 10.0]),
    point([-2.0, 10.0]),
    point([-2.0, 9.0]),
    point([8.0, 9.0]),
    point([8.0, 3.0]),
]

not_including_pocket = pocket([
    segment([a, b]) for a, b in all_two_elements(not_including)
])

if tested_pocket.is_included_in(including_pocket):
    print("red is included in green (ok)")
else:
    print("red is not included in green (not ok)")

if tested_pocket.is_included_in(not_including_pocket):
    print("red is not included in blue (ok)")
else:
    print("red is included in blue (not ok)")

tycat(tested_pocket, including_pocket, not_including_pocket)

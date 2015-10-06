#!/usr/bin/env python

from jimn.point import point
from jimn.displayable import tycat
from jimn.utils.iterators import all_two_elements
from jimn.segment import segment
from jimn.pocket import pocket
from jimn.polygon import polygon

# testing inclusion

square = polygon.square(-0.5, 5, 1)
tested_pocket = pocket(list(square.segments()))
square2 = polygon.square(0.5, 5, 1)
tested_pocket2 = pocket(list(square2.segments()))

including = polygon.square(-1, 4, 3)
including_pocket = pocket(list(including.segments()))

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

vertically_aligned = [
    point([-1, 3.5]),
    point([2, 3.5]),
    point([4, 5.5]),
    point([2, 7.5]),
    point([-1, 7.5]),
    point([-4, 5.5]),
]

va_pocket = pocket([
    segment([a, b]) for a, b in all_two_elements(vertically_aligned)
])

if tested_pocket.is_included_in(including_pocket):
    print("red is included in green (ok)")
else:
    print("red is not included in green (not ok)")

if tested_pocket.is_included_in(not_including_pocket):
    print("red is included in blue (not ok)")
else:
    print("red is not included in blue (ok)")

if including_pocket.is_included_in(va_pocket):
    print("green is included in purple (ok)")
else:
    print("green is not included in purple (not ok)")

if tested_pocket.is_included_in(tested_pocket):
    print("red is included in red (ok)")
else:
    print("red is not included in red (not ok)")

if tested_pocket.is_included_in(tested_pocket2):
    print("red is included in orange (not ok)")
else:
    print("red is not included in orange (ok)")


tycat(tested_pocket, including_pocket, not_including_pocket, va_pocket,
      tested_pocket2)

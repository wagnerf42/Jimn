#!/usr/bin/python3

from jimn.point import point
from jimn.segment import segment
from jimn.displayable import tycat


segments_test = []
# most common case
segments_test.append([[0.1, 0.1], [1.1, 1.1]])
# starting endpoint on slice center line
segments_test.append([[0.3, 0.3], [1.1, 1.1]])
# ending endpoint on slice center line
segments_test.append([[0.1, 0.1], [1.2, 1.2]])
# starting and ending endpoints on slice center lines
segments_test.append([[0.9, 0.9], [1.2, 1.2]])
# segment do not meet any slice center line
segments_test.append([[0.1, 1.0], [1.1, 1.1]])
# horizontal segment aligned on a slice center line
segments_test.append([[0.0, 1.2], [1.2, 1.2]])

segments_test = [[point(c1), point(c2)] for [c1, c2] in segments_test]
segments_test = [segment(endpoints) for endpoints in segments_test]

heights = [0.3 * n for n in range(0, 5)]
center_lines = [segment([point([0.0, h]), point([1.2, h])]) for h in heights]
for s in segments_test:
    elementary_segments = s.cut(0.3)
    print([str(e) for e in elementary_segments])
    tycat(center_lines, *elementary_segments)

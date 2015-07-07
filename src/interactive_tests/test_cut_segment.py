#!/usr/bin/python3

from collections import defaultdict
from jimn.point import point
from jimn.segment import segment


segments_test = []
segments_test.append([[0.1, 0.1], [1.1, 1.1]])
segments_test.append([[0.3, 0.3], [1.1, 1.1]])
segments_test.append([[0.1, 0.1], [1.2, 1.2]])
segments_test.append([[0.9, 0.9], [1.2, 1.2]])
segments_test.append([[1.0, 1.0], [1.1, 1.1]])
segments_test.append([[0.0, 1.2], [1.2, 1.2]])

segments_test = [[point(c1), point(c2)] for [c1, c2] in segments_test]
segments_test = [segment(endpoints) for endpoints in segments_test]
for s in segments_test:
    vertices_per_height = defaultdict(list)
    vertices = s.cut(0.3, vertices_per_height)
    print([str(v) for v in vertices])

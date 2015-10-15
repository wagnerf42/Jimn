#!/usr/bin/env python3
from jimn.point import point
from jimn.segment import segment
from jimn.pocket import pocket

coordinates = [
-1.0783959675963575, 0.0, -1.48, -0.16634439340048413,
-1.48, -0.16634439340048413, -1.48, 0.16634439340048413,
-1.48, 0.16634439340048413, -1.0783959675963575, 0.0
]

points = [point([coordinates[2*i], coordinates[2*i+1]]) for i in range(6)]
segments = [segment([points[2*i], points[2*i+1]]) for i in range(3)]
p1 = pocket(segments)
p2 = pocket(segments)

cache = {}
cache[p1] = 3
if p2 in cache:
	print("ok")
else:
	print("not ok")


#!/usr/bin/env python3

from random import random
from jimn.point import Point
from jimn.utils.coordinates_hash import CoordinatesHash


for prec in range(6, 7):
    chash = CoordinatesHash(wanted_precision=prec)
    max_distance = 0
    all_points = []
    min_distance = None
    for _ in range(5000):
        p = Point([random(), random()])
        hashed_p = chash.hash_point(p)
        if p != hashed_p:
            distance = p.distance_to(hashed_p)
            max_distance = max(distance, max_distance)
        else:
            if all_points:
                min_distance = min([p.distance_to(q) for q in all_points])
        all_points.append(p)

    print("max distance obtained for precision", prec, "is", max_distance)
    print("min distance obtained for precision", prec, "is", min_distance)

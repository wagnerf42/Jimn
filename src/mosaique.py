#!/usr/bin/env python3
from jimn.point import Point
from jimn.segment import Segment
from jimn.displayable import tycat
from jimn.polygon import Polygon
from jimn.utils.coordinates_hash import CoordinatesHash
from jimn.algorithms.bentley_ottmann import compute_intersections
from itertools import product
from random import random
from math import pi
from collections import defaultdict

def main():
    # create random segments
    segments = [Segment([Point([random() - 0.5, random() - 0.5]) for _ in range(2)]) for _ in range(5)]
    tycat(segments)
    center = Point([0.5, 0.5])

    # flip vertically
    flipped_segments = []
    for segment in segments:
        start, end = segment.endpoints
        flipped_start = Point([start.coordinates[0], 0.5 + abs(start.coordinates[1] - 0.5)])
        flipped_end = Point([end.coordinates[0], 0.5 + abs(end.coordinates[1] - 0.5)])
        flipped_segments.append(Segment([flipped_start, flipped_end]))
    tycat(flipped_segments, segments)
    segments.extend(flipped_segments)

    # rotate 8 times
    rotated_segments = [s.rotate_around(center, pi/4*i) for s in segments for i in range(8)]

    unit_square = Polygon.square(0, 0, 1)
    tycat(rotated_segments, center, unit_square)

    # clip into unit square
    clipped_segments = list(filter(None, (s.clip(center, 0.5) for s in rotated_segments)))
    tycat(clipped_segments, center, unit_square)

    # tile
    adjuster = CoordinatesHash()
    translations = filter(lambda x: x[0] != 0 or x[1] != 0, product(range(-1, 2), repeat=2))
    translated_segments = list(clipped_segments)
    for (x, y) in translations:
        translation = Point([x, y])
        for segment in clipped_segments:
            translated_segment = Segment([adjuster.hash_point(p+translation) for p in segment.endpoints])
            translated_segments.append(translated_segment)
    tycat(translated_segments, center, unit_square)

    # now, remove all points of degree 1
    degrees = defaultdict(int)
    for segment in translated_segments:
        for point in segment.endpoints:
            degrees[point] += 1

    # remaining_segments = 



main()

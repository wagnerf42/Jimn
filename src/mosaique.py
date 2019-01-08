#!/usr/bin/env python3
from jimn.point import Point
from jimn.segment import Segment
from jimn.displayable import tycat
from jimn.polygon import Polygon
from jimn.algorithms.bentley_ottmann import compute_intersections
from jimn.utils.precision import is_almost
from itertools import product
from random import random
from math import pi, floor
from collections import defaultdict

PREC = 0.001

class CHash:
    def __init__(self):
        self.coordinates = (dict(), dict())
    def hash_coordinate(self, c):
        hash_key = floor(c/PREC)
        displaced_key = floor((c+PREC/2.0)/PREC)
        if hash_key in self.coordinates[0]:
            c = self.coordinates[0][hash_key]
        elif displaced_key in self.coordinates[1]:
            c = self.coordinates[1][displaced_key]
        self.coordinates[0][hash_key] = c
        self.coordinates[1][displaced_key] = c
        return c

    def hash_point(self, point):
        return Point([self.hash_coordinate(c) for c in point.coordinates])

    def hash_segment(self, segment):
        return Segment([self.hash_point(p) for p in segment.endpoints])

ROUNDER2D = CHash()


def pairs(iterator):
    prec = None
    for e in iterator:
        if prec is not None:
            yield [prec, e]
        prec = e

def cut_segments(segments):
    """
    cut into elementary paths
    """
    small_segments = []
    for i, segment in enumerate(segments):
        intersections = list(
                ROUNDER2D.hash_point(p)
                for p in filter(
                    None,
                    (segment.intersection_with_segment(other) for other in segments if other is not segment)
                )
        )
        intersections.extend(segment.endpoints)
        if len(intersections) == 2:
            small_segments.append(segment)
        else:
            intersections = sorted(set(intersections))
            small_segments.extend(Segment(p) for p in pairs(intersections))
    return small_segments

def main():
    ROUNDER2D.hash_coordinate(0.5)
    ROUNDER2D.hash_coordinate(-0.5)
    origin = Point([0.0, 0.0])
    one = Point([1.0, 1.0])
    print("on cree des segments aleatoires")
    input()
    # create random segments
    segments = [Segment([ROUNDER2D.hash_point(Point([random(), random()])) for _ in range(2)]) for _ in range(7)]
    tycat(segments, origin, one)

    # flip random segments
    print("on symmetrise par rapport a x=0")
    input()
    flipped_segments = [Segment([ROUNDER2D.hash_point(Point([-p.coordinates[0], p.coordinates[1]])) for p in s.endpoints]) for s in segments]
    tycat(segments, flipped_segments, origin, one)
    segments.extend(flipped_segments)

    # rotate 8 times
    print("on tourne 8 fois")
    input()
    rotated_segments = [ROUNDER2D.hash_segment(s.rotate(pi/4*i)) for s in segments for i in range(8)]

    unit_square = Polygon.square(-0.5, -0.5, 1)
    tycat(segments, rotated_segments, origin, one, unit_square)

    # clip into unit square
    print("on garde ce qui est dans le carre unite centre en (0,0)")
    input()
    clipped_segments = list(ROUNDER2D.hash_segment(s) for s in filter(None, (s.clip(origin, 0.5) for s in rotated_segments)))
    tycat(clipped_segments, origin, unit_square)

    # cut into elementary segments
    print("on coupe en segments elementaires")
    input()
    clipped_segments = cut_segments(clipped_segments)
    tycat(clipped_segments, origin, unit_square)

    # tile
    print("on translate 8x")
    input()
    translations = filter(lambda x: x[0] != 0 or x[1] != 0, product(range(-1, 2), repeat=2))
    translated_segments = list(clipped_segments)
    for (x, y) in translations:
        translation = Point([x, y])
        for segment in clipped_segments:
            translated_segment = Segment([ROUNDER2D.hash_point(p+translation) for p in segment.endpoints])
            translated_segments.append(translated_segment)
    tycat(translated_segments, origin, unit_square)


    # now, remove all points of degree 1
    print("on elimine les degres 1")
    input()
    kept_segments = translated_segments
    while True:
        size = len(kept_segments)
        degrees = defaultdict(int)
        for segment in translated_segments:
            for point in segment.endpoints:
                degrees[point] += 1
        kept_segments = [s for s in translated_segments if all(degrees[p] > 1 for p in s.endpoints)]
        if size == len(kept_segments):
            break
    tycat(kept_segments, origin, unit_square)




main()

#!/usr/bin/env python3
"""
test kuhn munkres intersection algorithm.
"""
from math import cos, sin
import sys
from time import clock
from random import random, seed
from jimn.point import Point
from jimn.segment import Segment
from jimn.arc import Arc
from jimn.displayable import tycat
from jimn.algorithms.bentley_ottmann import compute_intersections
from jimn.utils.coordinates_hash import ROUNDER2D


def test(seconds=None):
    """
    intersect a bunch of random segments and display result.
    """
    display = True
    if seconds is None:
        display = False
        seconds = clock()

    seed(float(seconds))

    paths = [Segment([ROUNDER2D.hash_point(Point([random(), random()])),
                      ROUNDER2D.hash_point(Point([random(), random()]))])
             for _ in range(3)]
    for _ in range(0):
        center = ROUNDER2D.hash_point(Point([random(), random()]))
        radius = 0
        while radius < 0.02:
            radius = random()/4
        points = [center + ROUNDER2D.hash_point(
            Point([cos(a), sin(a)]) * radius)
                  for a in (random()*10, random()*10)]

        paths.append(Arc(radius, points, center).correct_endpoints_order())

    # print(",\n        ".join([str(s) for s in paths]))
    if display:
        tycat(paths)
    try:
        small_paths = compute_intersections(paths)
    except:
        print("seed", seconds)
        tycat(paths)
        raise

    return small_paths


def main():
    """
    automated tests or nice display
    """
    if len(sys.argv) > 1:
        print("using seed", sys.argv[1])
        tycat(*test(sys.argv[1]))
    else:
        for iteration in range(2000):
            print(iteration)
            test()
            ROUNDER2D.clear()
        print("done")


if __name__ == "__main__":
    main()

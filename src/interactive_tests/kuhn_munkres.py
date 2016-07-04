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
from jimn.algorithms.sweeping_line_algorithms.kuhn_munkres import kuhn_munkres
from jimn.utils.coordinates_hash import ROUNDER2D


def test(seconds=None):
    """
    intersect a bunch of random segments and display result.
    """
    if seconds is None:
        seconds = clock()

    seed(float(seconds))

    paths = [Segment([ROUNDER2D.hash_point(Point([random(), random()])),
                      ROUNDER2D.hash_point(Point([random(), random()]))])
             for _ in range(100)]
    for _ in range(100):
        center = ROUNDER2D.hash_point(Point([random(), random()]))
        radius = random()/4
        points = [center + ROUNDER2D.hash_point(
            Point([cos(a), sin(a)]) * radius)
                  for a in (random()*10, random()*10)]

        paths.append(Arc(radius, points, center).correct_endpoints_order())

    # print(",\n        ".join([str(s) for s in paths]))
    try:
        small_paths = kuhn_munkres(paths, cut_arcs=True)
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
        for iteration in range(200):
            print(iteration)
            test()
        print("done")


if __name__ == "__main__":
    main()

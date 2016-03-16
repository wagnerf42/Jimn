#!/usr/bin/env python3
"""
test kuhn munkres intersection algorithm.
"""
import sys
from time import clock
from random import random, seed
from jimn.point import Point
from jimn.segment import Segment
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

    segments = [Segment([ROUNDER2D.hash_point(Point([random(), random()])),
                         ROUNDER2D.hash_point(Point([random(), random()]))])
                for _ in range(20)]
    # print(",\n        ".join([str(s) for s in segments]))
    try:
        small_segments = kuhn_munkres(segments)
    except:
        print("seed", seconds)
        tycat(segments)
        raise

    return small_segments


def main():
    """
    automated tests or nice display
    """
    if len(sys.argv) > 1:
        print("using seed", sys.argv[1])
        tycat(*test(sys.argv[1]))
    else:
        for _ in range(500):
            test()
        print("done")


if __name__ == "__main__":
    main()

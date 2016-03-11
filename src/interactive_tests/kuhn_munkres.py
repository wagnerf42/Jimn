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
    segments = [
        Segment([Point([0.4241119488810712, 0.00019661343570498424]), Point([0.6535483322882256, 0.861510849309335])]),
        Segment([Point([0.5176460460243731, 0.8693823221023066]), Point([0.5672999885022317, 0.1944982127111603])]),
        Segment([Point([0.25488551620733346, 0.2224528880086054]), Point([0.8831873765374082, 0.8962599183710943])]),
        Segment([Point([0.8925229436358193, 0.11053296011034819]), Point([0.11521707482984123, 0.8529558894942431])]),
    ]
    try:
        small_segments = kuhn_munkres(segments)
    except:
        print("seed", seconds)
        tycat(segments)
        raise

    return (small_segments)


def main():
    """
    automated tests or nice display
    """
    if len(sys.argv) > 1:
        print("using seed", sys.argv[1])
        tycat(*test(sys.argv[1]))
    else:
        for _ in range(200):
            test()
        print("done")


if __name__ == "__main__":
    main()

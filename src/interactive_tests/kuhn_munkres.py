#!/usr/bin/env python3
"""
test kuhn munkres intersection algorithm.
"""
from time import clock
from random import random, seed
from jimn.point import Point
from jimn.segment import Segment
from jimn.displayable import tycat
from jimn.algorithms.sweeping_line_algorithms.kuhn_munkres import kuhn_munkres


def main():
    """
    intersect a bunch of random segments and display result.
    """
    seconds = clock()
    seconds = 0.069896
    print("seed:", seconds)
    seed(seconds)
    segments = [Segment([Point([random(), random()]),
                         Point([random(), random()])])
                for _ in range(200)]
    intersections = kuhn_munkres(segments)
    points = set()
    for local_points in intersections.values():
        points.update(local_points)
    tycat(segments, list(points))


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""
test kuhn munkres intersection algorithm.
"""
from random import random
from jimn.point import Point
from jimn.segment import Segment
from jimn.displayable import tycat
from jimn.algorithms.sweeping_line_algorithms.kuhn_munkres import kuhn_munkres


def main():
    """
    intersect a bunch of random segments and display result.
    """
    segments = [Segment([Point([random(), random()]),
                         Point([random(), random()])])
                for _ in range(200)]
    intersections = kuhn_munkres(segments)
    tycat(segments, list(intersections.values()))


if __name__ == "__main__":
    main()

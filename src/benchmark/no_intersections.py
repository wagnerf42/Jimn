#!/usr/bin/env python3
"""
profiles kuhn munkres on
    - varying number of segments
    - no intersections
    - all segments in treap at once / 1 in treap at once
"""
from time import clock
from jimn.point import Point
from jimn.segment import Segment
from jimn.algorithms.sweeping_line_algorithms.kuhn_munkres import kuhn_munkres
from jimn.displayable import gnuplot


def overlapping_run(size):
    """
    one run, of given size.
    return time taken.
    """
    paths = [Segment([Point([0, i]), Point([1, i+0.5])]) for i in range(size)]
    return run(paths)


def non_overlapping_run(size):
    """
    one run, of given size.
    return time taken.
    """
    paths = [Segment([Point([2*i, 0]), Point([2*i+1, 0])])
             for i in range(size)]
    return run(paths)


def run(paths):
    """
    run km on given paths.
    return time taken.
    """
    start_time = clock()
    kuhn_munkres(paths)
    end_time = clock()
    return end_time - start_time


def main():
    """
    launch all runs
    """
    times = [
        (s, overlapping_run(s), non_overlapping_run(s))
        for s in range(1, 1000, 50)]
    gnuplot(times)

if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""
profiles bentley ottmann on
    - varying number of segments
    - no intersections
    - all segments in treap at once / 1 in treap at once
"""
from itertools import combinations
from time import clock
from jimn.point import Point
from jimn.segment import Segment
from jimn.algorithms.bentley_ottmann import compute_intersections
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

def quadratic(size):
    """
    n^2 brute force algorithm
    """
    paths = [Segment([Point([2*i, 0]), Point([2*i+1, 0])])
             for i in range(size)]
    start_time = clock()
    for p_1, p_2 in combinations(paths, r=2):
        p_1.intersections_with(p_2)
    end_time = clock()
    return end_time - start_time

def run(paths):
    """
    run km on given paths.
    return time taken.
    """
    start_time = clock()
    compute_intersections(paths)
    end_time = clock()
    return end_time - start_time


def main():
    """
    launch all runs
    """
    times = [
        (s, overlapping_run(s), non_overlapping_run(s), quadratic(s))
        for s in range(1, 60, 5)]
    labels = ("overlap", "no-overlap", "bruteforce")
    gnuplot(labels, times)

if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""
benchark current version
"""
from random import random
from os import system
from time import clock
from jimn.point import Point
from jimn.utils.coordinates_hash import CoordinatesHash
from jimn import compute_milling_path
from jimn.stl import Stl

def main():
    """
    launches all benchmars ; logs on stdout
    """
    system("git rev-parse HEAD")
    start = clock()
    points = [Point([random(), random()]) for _ in range(100000)]
    end = clock()
    print("created", len(points), "points in", end-start)
    start = clock()
    rounder = CoordinatesHash(5)
    for point in points:
        rounder.hash_point(point)
    end = clock()
    print("hashed", len(points), "points in", end-start)
    start = clock()
    model = Stl("../test_files/cordoba.stl")
    model.compute_slices(0.3, Point([0.0, 0.0]))
    end = clock()
    print("slicing small file in", end-start)
    start = clock()
    model = Stl("../test_files/cordoba-very-large.stl")
    model.compute_slices(0.1, Point([0.0, 0.0]))
    end = clock()
    print("slicing large file in", end-start)
    start = clock()
    compute_milling_path("../test_files/cordoba.stl", 0.3, 0.3)
    end = clock()
    print("easy instance in", end-start)
    start = clock()
    compute_milling_path("../test_files/cordoba-very-large.stl", 0.1, 0.1)
    end = clock()
    print("difficult instance in", end-start)


main()

#!/usr/bin/env python3
from jimn.point import Point
from jimn.segment import Segment
from jimn.algorithms.bentley_ottmann import compute_intersections

def main():
    s1 = Segment([Point([0,0]), Point([1,1])])
    s2 = Segment([Point([0,0]), Point([1,-1])])
    s3 = Segment([Point([0.5,-2]), Point([0.5,2])])
    compute_intersections([s1, s2, s3])


main()

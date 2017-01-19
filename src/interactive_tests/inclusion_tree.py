#!/usr/bin/env python3
"""
test inclusion tree builder
"""

from jimn.polygon import Polygon
from jimn.displayable import tycat
from jimn.algorithms.inclusion_tree_builder import build_inclusion_tree


def test_one_level():
    """
    very basic test with all polygons at same height
    """
    outside_square = Polygon.square(0, 0, 10)
    inner_square = Polygon.square(2, 2, 2)
    inner_square2 = Polygon.square(6, 6, 2)
    inner_inner = Polygon.square(2.1, 2.1, 0.3)
    polygons = {0: [outside_square, inner_square, inner_square2, inner_inner]}
    tycat(polygons[0])
    print("outside square", id(outside_square))
    print("inner square (top left)", id(inner_square))
    print("inner square (bottom right)", id(inner_square2))
    print("inner inner square (inside top left)", id(inner_inner))
    tree = build_inclusion_tree(polygons)
    tree.tycat()


test_one_level()

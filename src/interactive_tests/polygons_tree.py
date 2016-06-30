#!/usr/bin/env python3
"""
small test for inclusion trees and polygon trees
"""

from jimn.point import Point
from jimn.polygon import Polygon
from jimn.displayable import tycat
from jimn.tree.polygon_tree import PolygonTree
from jimn.utils.debug import add_module_to_debug


def test():
    """
    small examples with holes and fillings.
    """
    outer_shell = [
        Point([0.0, 0.0]),
        Point([9.0, 0.0]),
        Point([9.0, 8.0]),
        Point([0.0, 8.0]),
    ]

    big_hole = [
        Point([1.0, 5.0]),
        Point([4.0, 3.0]),
        Point([8.0, 5.0]),
        Point([4.0, 7.0])
    ]

    small_fill1 = [
        Point([2.0, 4.5]),
        Point([3.0, 4.5]),
        Point([3.0, 5.5]),
        Point([2.0, 5.5])
    ]

    small_fill2 = [
        Point([5.0, 4.5]),
        Point([6.0, 4.5]),
        Point([6.0, 5.5]),
        Point([5.0, 5.5])
    ]

    small_hole = [
        Point([6.0, 1.0]),
        Point([7.0, 1.0]),
        Point([7.0, 2.0]),
        Point([6.0, 2.0])
    ]

    polygons = {}
    polygons[4] = [Polygon(list(outer_shell))]
    polygons[3] = [Polygon(list(outer_shell))]
    polygons[2] = [
        Polygon(list(outer_shell)),
        Polygon(list(big_hole)),
        Polygon(list(small_fill1)),
        Polygon(list(small_fill2)),
        Polygon(list(small_hole))
    ]
    polygons[1] = [
        Polygon(list(outer_shell)),
        Polygon(list(big_hole)),
        Polygon(list(small_fill1)),
        Polygon(list(small_hole))
    ]

    for height in sorted(polygons.keys(), reverse=True):
        print("height:", height)
        tycat(polygons[height])

    add_module_to_debug(
        "jimn.algorithms.sweeping_line_algorithms.inclusion_tree_builder")
    add_module_to_debug("jimn.tree.polygon_tree")
    tree = PolygonTree.build(0.01, polygons)
    print("simulating depth first carving")
    tree.display_depth_first()

test()

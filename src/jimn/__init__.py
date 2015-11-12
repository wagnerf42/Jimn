from jimn.utils.debug import is_module_debugged
from jimn.displayable import tycat
from jimn.stl import stl
from jimn.tree.polygon_tree import polygon_tree
from jimn.pocket.builder import build_polygons
from jimn.tree.pocket_tree import pocket_tree
from jimn.tree.path_tree import path_tree
from jimn.algorithms.segment_merger import merge_segments
from jimn.utils.coordinates_hash import rounder2d
from math import floor, ceil
import sys


def compute_milling_path(stl_file, slice_size, milling_radius):
    """
    main procedure.
    loads stl file ;
    cuts in slices of thickness 'slice_size' ;
    compute polygons and offset them with milling radius ;
    returns global milling path
    """
    if __debug__:
        if is_module_debugged(__name__):
            print("computing path ; thickness is", slice_size, "radius is",
                  milling_radius)

    model = stl(stl_file)
    border = model.border_2d()
    # hash all x coordinates used in milling
    # we need to hash them all before creating any 2d point
    # to ensure no points falls very near a milling line
    _hash_milling_heights(border[0].get_endpoint(0).get_y(),
                          border[0].get_endpoint(1).get_y(), milling_radius * 2)

    if __debug__:
        if is_module_debugged(__name__):
            print("model loaded")
            print("slices are:")

    slices = model.compute_slices(slice_size)

    slices_polygons = {}  # polygons in each slice, indexed by height

    for height in sorted(slices):
        stl_slice = slices[height]
        stl_slice.extend(border)
        if __debug__:
            if is_module_debugged(__name__):
                tycat(stl_slice)
        simpler_slice = merge_segments(stl_slice)
        slice_polygons = build_polygons(simpler_slice)
        slices_polygons[height] = slice_polygons

    if __debug__:
        if is_module_debugged(__name__):
            print("building polygon tree")
    tree = polygon_tree.build(slices_polygons)
    if __debug__:
        if is_module_debugged(__name__):
            tree.tycat()
            print("building pockets tree")
    pockets = pocket_tree.build(tree, milling_radius)
    if pockets.is_empty():
        print("nothing left : milling radius is too high !")
        sys.exit()
    if __debug__:
        if is_module_debugged(__name__):
            pockets.tycat()
            print("building paths tree")
    paths = path_tree.build(pockets, milling_radius)
    if __debug__:
        if is_module_debugged(__name__):
            paths.tycat()
            print("merging all paths")
    return paths.global_path(milling_radius)


def _hash_milling_heights(ymin, ymax, milling_diameter):
    start = floor(ymin / milling_diameter)
    end = ceil(ymax / milling_diameter)
    for i in range(start, end+1):
        rounder2d.hash_coordinate(1, i * milling_diameter)

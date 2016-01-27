"""
main jimn file. compute path out of stl file.
"""

import sys
from jimn.utils.debug import is_module_debugged
from jimn.displayable import tycat
from jimn.stl import Stl
from jimn.tree.polygon_tree import PolygonTree
from jimn.pocket.builder import build_polygons
from jimn.tree.pocket_tree import pocket_tree
from jimn.tree.path_tree import path_tree
from jimn.vertical_path import VerticalPath
from jimn.algorithms.segment_merger import merge_segments


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

    VerticalPath.milling_height = slice_size
    slices_polygons = slice_stl_file(stl_file, slice_size)
    tree = build_polygons_tree(milling_radius, slices_polygons)
    pockets = build_pockets_tree(milling_radius, tree)
    paths = build_paths_tree(milling_radius, pockets)

    if __debug__:
        if is_module_debugged(__name__):
            print("merging all paths")
    return paths.global_path(milling_radius)


def slice_stl_file(stl_file, slice_size):
    """
    load stl file. cut into slices of wanted size and build polygons.
    return polygons arrays indexed by slice height.
    """
    model = Stl(stl_file)
    border = model.border_2d()

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

    return slices_polygons


def build_polygons_tree(milling_radius, slices_polygons):
    """
    turn slices into polygons tree.
    """
    if __debug__:
        if is_module_debugged(__name__):
            print("building polygon tree")
    tree = PolygonTree.build(milling_radius, slices_polygons)
    if __debug__:
        if is_module_debugged(__name__):
            tree.tycat()
    return tree


def build_pockets_tree(milling_radius, tree):
    """
    transform polygons tree into pockets tree.
    """
    if __debug__:
        if is_module_debugged(__name__):
            print("building pockets tree")
    pockets = pocket_tree.build(tree, milling_radius)
    if pockets.is_empty():
        print("nothing left : milling radius is too high !")
        sys.exit()
    if __debug__:
        if is_module_debugged(__name__):
            pockets.tycat()
    return pockets


def build_paths_tree(milling_radius, pockets):
    """
    transform pockets tree into paths tree.
    """
    if __debug__:
        if is_module_debugged(__name__):
            print("building paths tree")
    paths = path_tree.build(pockets, milling_radius)
    if __debug__:
        if is_module_debugged(__name__):
            paths.tycat()
    return paths

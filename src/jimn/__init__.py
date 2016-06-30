"""
jimn is a geometry module intended for computing paths for CNC machines.
it however can still be used as a standalone module for doing geometry.

you can find standard graphical objects xxx in sub-modules jimn.xxx like
    - point
    - segment
    - arc
    - polygon
    - circle
    - facet (3D)
    - holed_polygon

jimn is intended to work with the terminology terminal emulator from
enlightenment. under terminology any object or list of objects can be
graphically displayed using the tycat function from jimn.displayable.


you can have a guided tour for many modules by executing them directly.

for example a tour of jimn.point can be seen when executing the
jimn/point.py file under python3.
"""

import sys
from jimn.utils.debug import is_module_debugged
from jimn.displayable import tycat
from jimn.point import Point
from jimn.segment import Segment
from jimn.utils.iterators import all_two_elements
from jimn.stl import Stl
from jimn.tree.polygon_tree import PolygonTree
from jimn.pocket.builder import build_polygons
from jimn.tree.pocket_tree import PocketTree
from jimn.tree.path_tree import PathTree
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
    slices_polygons = slice_stl_file(stl_file, slice_size, milling_radius)
    tree = build_polygons_tree(milling_radius, slices_polygons)
    pockets = build_pockets_tree(milling_radius, tree)
    paths = build_paths_tree(milling_radius, pockets)

    if __debug__:
        if is_module_debugged(__name__):
            print("merging all paths")
    return paths.global_path(milling_radius)


def slice_stl_file(stl_file, slice_size, milling_radius):
    """
    load stl file. cut into slices of wanted size and build polygons.
    return polygons arrays indexed by slice height.
    """
    model = Stl(stl_file)
    margin = 2 * milling_radius + 0.01
    border = border_2d(model, margin)

    if __debug__:
        if is_module_debugged(__name__):
            print("model loaded")
            print("slices are:")

    slices = model.compute_slices(slice_size, model.translation_vector(margin))

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
    pockets = PocketTree.build(tree, milling_radius)
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
    paths = PathTree.build(pockets, milling_radius)
    if __debug__:
        if is_module_debugged(__name__):
            paths.tycat()
    return paths


def border_2d(stl_model, margin):
    """
    return 2d enclosing (starting at origin) for given model and margin.
    """
    # build four points
    xmin, ymin = 0, 0
    xmax, ymax = stl_model.dimensions(margin)
    points = []
    points.append(Point([xmin, ymin]))
    points.append(Point([xmin, ymax]))
    points.append(Point([xmax, ymax]))
    points.append(Point([xmax, ymin]))

    return [Segment([p, q]) for p, q in all_two_elements(points)]

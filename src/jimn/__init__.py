from jimn.utils.debug import is_module_debugged
from jimn.displayable import tycat
from jimn.stl import stl
from jimn.polygontree.polygontree_builder import build_tree
from jimn.pocket.builder import build_polygons
from jimn.algorithms.segment_merger import merge_segments


def compute_carving_path(stl_file, slice_size, carving_radius):
    """
    main procedure.
    loads stl file ;
    cuts in slices of thickness 'slice_size' ;
    compute polygons and offset them with carving radius ;
    returns global carving path
    """
    if __debug__:
        if is_module_debugged(__name__):
            print("computing path ; thickness is", slice_size, "radius is",
                  carving_radius)

    model = stl(stl_file)
    if __debug__:
        if is_module_debugged(__name__):
            print("model loaded")

    slices = model.compute_slices(slice_size)

    slices_polygons = {}  # polygons in each slice, indexed by height
    border = model.border_2d()

    for height, stl_slice in slices.items():
        stl_slice.extend(border)
        if __debug__:
            if is_module_debugged(__name__):
                tycat(stl_slice)
        simpler_slice = merge_segments(stl_slice)
        slice_polygons = build_polygons(simpler_slice)
        slices_polygons[height] = slice_polygons

    tree = build_tree(slices_polygons)
    pockets_tree = tree.offset_polygons(carving_radius)
    return pockets_tree.compute_path()

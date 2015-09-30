from jimn.utils.debug import is_module_debugged
from jimn.displayable import tycat
from jimn.stl import stl
from jimn.tree.polygon_tree import polygon_tree
from jimn.pocket.builder import build_polygons
from jimn.tree.pocket_tree import pocket_tree
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

    tree = polygon_tree.build(slices_polygons)
    pockets = pocket_tree.build(tree, carving_radius)
    paths = path_tree.build(pockets, carving_radius)
    return paths.global_path(carving_radius)

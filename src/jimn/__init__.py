from jimn.stl import stl
from jimn.polygontree.polygontree_builder import build_tree
from jimn.algorithms.poly_builder import build_polygons
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
        print("computing path ; thickness is", slice_size, "radius is",
              carving_radius)

    model = stl(stl_file)
    slices = model.compute_slices(slice_size)
    slices_polygons = {}  # polygons in each slice, indexed by height
    border = model.border_2d()

    for height, stl_slice in slices.items():
        stl_slice.extend(border)
        simpler_slice = merge_segments(stl_slice)
        slice_polygons = build_polygons(simpler_slice)
        slices_polygons[height] = slice_polygons

    tree = build_tree(slices_polygons)
    tree.offset_polygons(carving_radius)
    return tree.compute_path()

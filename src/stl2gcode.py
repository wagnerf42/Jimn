#!/usr/bin/env python3

import sys

from jimn.stl import stl
from jimn.displayable import tycat_set_svg_dimensions
from jimn.polygontree import polygontree
from jimn.poly_builder import polygonbuilder
from jimn.inclusion_tree_builder import inclusion_tree_builder
from jimn.segment_merger import segment_merger
from jimn.displayable import tycat
from jimn.polygontree_builder import polygontree_builder

# tycat_set_svg_dimensions(1024, 768)
(bin_name, stl_file, slice_size) = sys.argv
print("stl file : {}".format(stl_file))
print("slice size : {}".format(slice_size))

model = stl(stl_file)

slices = model.compute_slices(float(slice_size))
slices_polygons = {}
border = model.border_2d()
for height, stl_slice in slices.items():
    stl_slice.extend(border)
    merger = segment_merger(stl_slice)
    simpler_slice = merger.merge()
    builder = polygonbuilder(simpler_slice)
    slice_polygons = builder.build_polygons()
    slices_polygons[height] = slice_polygons

itree = polygontree_builder(slices_polygons)

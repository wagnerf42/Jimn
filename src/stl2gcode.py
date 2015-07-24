#!/usr/bin/env python3

import sys

from jimn.stl import stl
from jimn.displayable import tycat
from jimn.algorithms.poly_builder import build_polygons
from jimn.algorithms.segment_merger import merge_segments

(bin_name, stl_file, slice_size) = sys.argv
print("stl file : {}".format(stl_file))
print("slice size : {}".format(slice_size))

model = stl(stl_file)

slices = model.compute_slices(float(slice_size))
slices_polygons = {}
border = model.border_2d()
for height, stl_slice in slices.items():
    stl_slice.extend(border)
    simpler_slice = merge_segments(stl_slice)
    slice_polygons = build_polygons(simpler_slice)
    slices_polygons[height] = slice_polygons

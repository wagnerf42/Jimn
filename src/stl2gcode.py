#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

import sys

from jimn.stl import stl
from jimn.displayable import tycat_set_svg_dimensions
from jimn.polygontree import polygontree
from jimn.poly_builder import polygonbuilder
from jimn.segment_merger import segment_merger
from jimn.displayable import tycat

# tycat_set_svg_dimensions(1024, 768)
(bin_name, stl_file, slice_size) = sys.argv
print("stl file : {}".format(stl_file))
print("slice size : {}".format(slice_size))

model = stl(stl_file)

slices = model.compute_slices(float(slice_size))
slices_polygons = []
border = model.border_2d()
for stl_slice in slices:
    stl_slice.extend(border)
    merger = segment_merger(stl_slice)
    simpler_slice = merger.merge()
    builder = polygonbuilder(simpler_slice)
    slice_polygons = builder.build_polygons()
    tycat(*slice_polygons)
    slices_polygons.append(slice_polygons)

ptree = polygontree(slices_polygons)

#!/usr/bin/env python3

from jimn.stl import stl
from jimn.displayable import tycat_set_svg_dimensions
from jimn.segment_merger import segment_merger
import sys

(bin_name, stl_file) = sys.argv
model = stl(stl_file)
border = model.border_2d()
flat = model.flatten()
merger = segment_merger(flat)
simpler_slice = merger.merge()
if len(simpler_slice) > 0:
    print("incorrect input : holes in surface")
    tycat(simpler_slice, border)
else:
    print("input validated")

#!/usr/bin/env python3

from jimn.stl import Stl
from jimn.displayable import tycat
from jimn.algorithms.segment_merger import merge_segments
import sys

(bin_name, stl_file) = sys.argv
model = stl(stl_file)
border = model.border_2d()
flat = model.flatten()
simpler_slice = merge_segments(flat)
if len(simpler_slice) > 0:
    print("incorrect input : holes in surface")
    tycat(simpler_slice, border)
else:
    print("input validated")

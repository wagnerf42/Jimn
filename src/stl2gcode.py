#!/usr/bin/env python3
"""
generate gcode out of stl file
"""

import sys
from jimn import compute_milling_path

if len(sys.argv) < 4:
    print("please give following arguments: stl_file," +
          " slice_thickness, milling_radius")
    sys.exit()

(bin_name, stl_file, slice_size, milling_radius) = sys.argv[0:4]

path = compute_milling_path(stl_file, float(slice_size),
                            float(milling_radius))

if len(sys.argv) > 4:
    path.animate(float(milling_radius))
print("done")

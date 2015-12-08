#!/usr/bin/env python3

from jimn import compute_milling_path
import sys

if len(sys.argv) != 4:
    print("please give following arguments: stl_file, slice_thickness, milling_radius")
    sys.exit()

(bin_name, stl_file, slice_size, milling_radius) = sys.argv

p = compute_milling_path(stl_file, float(slice_size),
                         float(milling_radius))
p.animate()
print("done")

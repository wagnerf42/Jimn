#!/usr/bin/env python3

from jimn import compute_carving_path
import sys

(bin_name, stl_file, slice_size, carving_radius) = sys.argv

p = compute_carving_path(stl_file, float(slice_size),
                         float(carving_radius))

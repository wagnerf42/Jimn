#!/usr/bin/env python3
# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

import sys

from jimn.stl import stl
from jimn.polygontree import polygontree

(bin_name, stl_file, slice_size) = sys.argv
print("stl file : {}".format(stl_file))
print("slice size : {}".format(slice_size))

model = stl(stl_file)

ptree = polygontree(model, float(slice_size))

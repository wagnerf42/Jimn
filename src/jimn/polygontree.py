# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.displayable import tycat
from jimn.poly_builder import polygonbuilder


class polygontree:
    def __init__(self, model, slice_size):
        self.slices = model.compute_slices(slice_size)
        for stl_slice in self.slices:
            builder = polygonbuilder(stl_slice)
            print('input')
            tycat(stl_slice)
            slice_polygons = builder.build_polygons()
            print('result')
            tycat(*slice_polygons)

# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.displayable import tycat
from jimn.poly_builder import polygonbuilder
from jimn.segment_merger import segment_merger


class polygontree:
    def __init__(self, model, slice_size):
        self.slices = model.compute_slices(slice_size)
        #TODO: careful : last slice is empty
        for stl_slice in self.slices:
            # due to roundings of coordinates we might have overlapping segments
            # remove them
            print('input')
            tycat(stl_slice)
            merger = segment_merger(stl_slice)
            simpler_slice = merger.merge()
            builder = polygonbuilder(simpler_slice)
            slice_polygons = builder.build_polygons()
            print('result')
            tycat(*slice_polygons)

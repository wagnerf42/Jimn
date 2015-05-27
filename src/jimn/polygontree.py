# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.displayable import tycat

class polygontree:
    def __init__(self, model, slice_size):
        self.slices = model.compute_slices(slice_size)
        # tycat(*self.slices)

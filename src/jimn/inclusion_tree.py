# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4


class inclusion_tree:
    """stores a set of polygons included one inside another"""
    def __init__(self, contained_polygon):
        self.polygon = contained_polygon
        self.children = []

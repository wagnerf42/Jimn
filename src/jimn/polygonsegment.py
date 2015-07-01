# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.segment import segment
from jimn.precision import check_precision


class polygonsegment(segment):
    """two additional attributes to basic segments : height and polygon id"""

    def __init__(self, points, height, polygon_we_belong_to):
        self.endpoints = sorted(points)
        if __debug__:
            for p in self.endpoints:
                assert p.dimension() == 2, 'polygonsegment works only on 2d points'
        self.height = height
        self.polygon = polygon_we_belong_to

    def vertical_intersection_at(self, x):
        x1, y1 = self.endpoints[0].get_coordinates()
        x2, y2 = self.endpoints[1].get_coordinates()
        if x1 == x2:
            # when vertical, we return lowest coordinate
            return y1
        if __debug__:
            check_precision(x1, x2, 'vertical_intersection_at')
        a = (y2-y1)/(x2-x1)
        y = y1 + a*(x-x1)
        return y

    def get_polygon_id(self):
        return id(self.polygon)

    def get_polygon(self):
        return self.polygon

    def get_height(self):
        return self.height

    def __eq__(a, b):
        return a.endpoints == b.endpoints

    # we assume segments never intersect
    # if segments overlap, use height for comparison
    # if segments don't overlap return true if a is located above and higher than b
    # in svg
    # never compare two segments which dont have intersection ranges for x coordinates
    def __ge__(a, b):
        x1max = max([s.endpoints[0].get_x() for s in (a, b)])
        x2min = min([s.endpoints[1].get_x() for s in (a, b)])
        common_abs = [x1max, x2min]
        ya, yb = [[s.vertical_intersection_at(x) for x in common_abs] for s in (a, b)]

        if __debug__:
            if ya == yb:
                assert x1max != x2min, 'comparing segments whose common absciss range is only one point'
                assert a.height != b.height, 'overlapping segments in the same slice'
            else:
                for yaa, ybb in zip(ya, yb):
                    if yaa != ybb:
                        check_precision(yaa, ybb, 'polygonsegment_lt')

        return ya <= yb

    def __hash__(self):
        to_hash = list(self.endpoints)
        to_hash.append(self.height)
        return hash(tuple(to_hash))

    def __str__(self):
        s = segment.__str__(self)
        s += str(self.get_height())
        return s

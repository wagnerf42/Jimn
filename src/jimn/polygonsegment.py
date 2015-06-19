# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.segment import segment
from jimn.precision import check_precision


class polygonsegment(segment):
    """two additional attributes to basic segments : height and polygon id"""

    def __init__(self, points, height, polygon_id):
        self.endpoints = sorted(points)
        if __debug__:
            for p in self.endpoints:
                assert p.dimension() == 2, 'polygonsegment works only on 2d points'
        self.height = height
        self.polygon_id = polygon_id

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

    def sort_endpoints(self):
        sorted_endpoints = sorted(self.endpoints, key=lambda p: (p.get_x(), p.get_y()))
        return polygonsegment(sorted_endpoints, self.height, self.polygon_id)

    def get_polygon_id(self):
        return self.polygon_id

    def get_height(self):
        return self.height

    def __eq__(a, b):
        return a.endpoints == b.endpoints

    # we assume segments never intersect
    # if segments overlap, use height for comparison
    # if segments don't overlap return true if a is located above b
    # in svg
    # never compare two segments which dont have intersection ranges for x coordinates
    def __lt__(a, b):
        x = max([s.endpoints[0].get_x() for s in (a, b)])
        ya, yb = [s.vertical_intersection_at(x) for s in (a, b)]

        if a.height < b.height:
            return False
        elif ya == yb:
            x2 = max([s.endpoints[1].get_x() for s in (a, b)])
            y2a, y2b = [s.vertical_intersection_at(x2) for s in (a, b)]
            if y2a == y2b:
                assert a.height != b.height, 'compared segments should not intersect'
                return a.height > b.height # toujours True...
            else:
                return y2a < y2b
        else:
            if __debug__:
                check_precision(ya, yb, 'polygonsegment_lt')
            return ya < yb

    def __hash__(self):
        to_hash = list(self.endpoints)
        to_hash.append(self.height)
        return hash(tuple(to_hash))

    def __str__(self):
        s = segment.__str__(self)
        s += str(self.get_height())
        return s

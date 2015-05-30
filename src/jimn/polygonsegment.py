# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.segment import segment
import sys


class polygonsegment(segment):
    """two additional attributes to basic segments : height and polygon id"""

    def __init__(self, points, height, polygon_id):
        self.endpoints = sorted(points)
        if __debug__:
            for p in self.endpoints:
                if len(p.get_coordinates()) != 2:
                    raise ValueError
        self.height = height
        self.polygon_id = polygon_id

    def vertical_intersection_at(self, x):
        x1, y1 = self.endpoints[0].get_coordinates()
        x2, y2 = self.endpoints[1].get_coordinates()
        if x1 == x2:
            # when vertical, we return lowest coordinate
            return y1
        if __debug__:
            if abs(x2 - x1) < 0.00001:
                print('warning: potential precision problem', file=sys.stderr)
        y = y1 + (x2-x)/(x2-x1)*(y2-y1)
        return y

    def __eq__(a, b):
        return a.endpoints == b.endpoints

    # we assume segments never intersect
    # if segments overlap, use height for comparison
    # if segments don't overlap return true if a is located above b
    # in svg
    # never compare two segments which dont have intersection ranges for x coordinates
    def __lt__(a, b):
        x = max([s.endpoints[0].get_x() for s in (a,b)])
        ya, yb = [s.vertical_intersection_at(x) for s in (a,b)]

        if ya == yb:
            if __debug__:
                if a.height == b.height:
                    raise ValueError
            return a.height > b.height
        else:
            if __debug__:
                if ya == yb:
                    raise ValueError
                if abs(ya - yb) < 0.00001:
                    print('warning: potential precision problem', file=sys.stderr)
            return ya < yb

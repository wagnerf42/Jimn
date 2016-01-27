"""
extend segment class with additional attributes.
"""
from jimn.segment import Segment
from jimn.utils.precision import check_precision


class PolygonSegment(Segment):
    """
    two additional attributes to basic segments : height and polygon id.
    """

    def __init__(self, segment, height, polygon_we_belong_to):
        super().__init__(sorted(segment.endpoints))
        self.height = height
        self.polygon = polygon_we_belong_to

    def split_at(self, points):
        """
        split at list of given points.
        require points to be all strictly differents and
        strictly differents from endpoints.
        """
        points.extend(self.endpoints)
        sorted_points = sorted(points)
        segments = [
            PolygonSegment([p1, p2], self.height, self.polygon)
            for p1, p2 in zip(
                sorted_points[:-1],
                sorted_points[1:]
            )
        ]
        return segments

    def get_polygon_id(self):
        return id(self.polygon)

    def __eq__(self, other):
        # TODO: why no poly comparisons ? -> who uses that ?
        return self.endpoints == other.endpoints

    def __ge__(self, other):
        # we assume segments never intersect.
        # if segments overlap, use height for comparison.
        # if segments don't overlap return true
        # if self is located above and higher than other in svg.
        # never compare two segments which dont have intersection ranges
        # for x coordinates.
        x1max = max([s.endpoints[0].get_x() for s in (self, other)])
        x2min = min([s.endpoints[1].get_x() for s in (self, other)])
        assert x1max != x2min, \
            'comparing segments whose common absciss range is only one point'
        common_abs = [x1max, x2min]
        # TODO: the doc here is not what we do
        # TODO: factorize with elementary path methods
        self_ys, other_ys = [
            [s.vertical_intersection_at(x) for x in common_abs]
            for s in (self, other)
        ]

        if __debug__:
            if self_ys == other_ys:
                assert self.height != other.height, \
                    'overlapping segments in the same slice'
            else:
                for self_y, other_y  in zip(self_ys, other_ys):
                    if self_y != other_y:
                        check_precision(self_y, other_y, 'polygonsegment_lt')

        return self_ys <= other_ys

    def __hash__(self):
        # TODO : anyone using that ?
        to_hash = list(self.endpoints)
        to_hash.append(self.height)
        return hash(tuple(to_hash))

    def __str__(self):
        string = Segment.__str__(self)
        string += str(self.height)
        return string

"""
extend segment class with additional attributes.
"""
from jimn.segment import Segment


class PolygonSegment(Segment):
    """
    two additional attributes to basic segments : height and polygon id.
    """

    def __init__(self, segment, height, polygon_we_belong_to):
        super().__init__(sorted(segment.endpoints))
        self.height = height
        self.polygon = polygon_we_belong_to

    def get_polygon_id(self):
        """
        return id of polygon we belong to.
        """
        return id(self.polygon)

    def __eq__(self, other):
        """
        comparison operator.
        used for path removal in sweeping line algorithms.
        """
        if self.height != other.height:
            return False
        if super().__eq__(other):
            assert id(self.polygon) == id(other.polygon)
            return True
        else:
            return False

    def __ge__(self, other):
        # we assume segments never intersect.
        # if segments overlap, use height for comparison.
        # if segments don't overlap return true
        # if self is located above and higher than other in svg.
        # never compare two segments which dont have intersection ranges
        # for x coordinates.
        aboveness = self.height_comparison(other)
        if aboveness == 0:  # overlap
            return self.height > other.height
        return aboveness == 1

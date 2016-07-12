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

    def sweeping_key(self, current_x):
        """
        return key used for comparing polygonsegments in sweeping
        line algorithms.
        """
        basic_key = super().sweeping_key(current_x)
        return (basic_key[0], basic_key[1], basic_key[2], -self.height)

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

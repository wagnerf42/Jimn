"""
extend segment class with additional attributes.
"""
from jimn.segment import Segment
from jimn.utils.iterators import all_two_elements


def polygon_segments(height, polygon):
    """
    iterates on all PolygonSegment inside a given polygon at
    a given height
    """
    for start, end in all_two_elements(polygon.points):
        yield PolygonSegment([start, end], height, polygon)


class PolygonSegment(Segment):
    """
    two additional attributes to basic segments : height and polygon we belong to.
    """
    def __init__(self, points, height, polygon_we_belong_to):
        super().__init__(points)
        self.height = height
        self.polygon = polygon_we_belong_to

    def polygon_id(self):
        """
        return id of polygon we belong to.
        """
        return id(self.polygon)

"""
base class for segments or arcs.
basic path between two endpoints (oriented).
"""
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import is_almost, SEGMENT_LIMIT


def set_comparer(new_comparer):
    """
    set object used for comparing paths
    """
    ElementaryPath.comparer = new_comparer


class ElementaryPath:
    """
    elementary path is a small path between two endpoints.
    class is further refined into segments and arcs.
    """
    comparer = None

    def __init__(self, points):
        self.endpoints = points
        assert self.endpoints[0].distance_to(self.endpoints[1]) \
            > SEGMENT_LIMIT, "very small path"

    def get_endpoint_not(self, point):
        """
        return endpoint not given point.
        requires path to have one endpoint being given point.

        example:
            point1 = Point([1, 2])
            point2 = Point([3, 4])
            point2 = Point([0, 0])
            segment = Segment([point1, point2])
            segment.get_endpoint_not(point1) -> returns point2
            segment.get_endpoint_not(point2) -> returns point1
            segment.get_endpoint_not(point3) -> raises exception
        """
        if self.endpoints[0] == point:
            return self.endpoints[1]
        if self.endpoints[1] == point:
            return self.endpoints[0]
        raise Exception("no given endpoint ; cannot find other")

    def distance_from_start(self, point):
        """
        return a scalar used for comparing points on path.
        the higher the scalar, the closer to endpoint.
        example:
            point1 = Point([1, 1])
            point2 = Point([5, 1])
            point3 = Point([3, 1])
            point4 = Point([4, 1])
            segment = Segment([point1, point2])
            # is point3 encounter after point4
            # when going from point1 to point ?
            if segment.distance_from_start(point3) > \
                    segment.distance_from_start(point4):
        """
        return self.endpoints[0].distance_to(point)

    def split_at(self, points):
        """
        split path at given points.
        returns list of same type objects ;
        orientation is kept ;
        assumes points are on path ;
        input points can be duplicated but no output paths are.

        example:

        points = [Point([c, c]) for c in range(4)]
        segment = Segment([points[0], points[3]])
        small_segments = segment.split_at(points)
        # small_segments contains segments (0,0) -- (1,1)
                                           (1,1) -- (2,2)
                                           (2,2) -- (3,3)
                                           (3,3) -- (4,4)
        """
        # pylint: disable=no-member
        points = set([p for a in (self.endpoints, points) for p in a])
        sorted_points = sorted(points, key=self.distance_from_start)

        paths = []
        for point1, point2 in zip(sorted_points[:-1], sorted_points[1:]):
            path_chunk = self.copy()
            path_chunk.endpoints[0] = point1.copy()
            path_chunk.endpoints[1] = point2.copy()
            paths.append(path_chunk)

        if __debug__:
            if is_module_debugged(__name__):
                print("splitting path:")
                tycat(self, *paths)
        return paths

    def split_around(self, intermediate_point):
        """
        return two subpaths: getting to intermediate and from intermediate
        to end. if intermediate is one endpoint, one of the returned path
        will be 'None'.
        pre-condition : intermediate_point is on path.
        example 1:
            segment = Segment(Point([0, 0]), Point([3, 3]))
            start, end = segment.split_around(segment, Point([1, 1]))
            # start is (0,0) -- (1,1) and end (1,1) -- (3,3)
            start, end = segment.split_around(segment, Point([0, 0]))
            # start is None and end (0,0) -- (3,3)
        """
        # pylint: disable=no-member
        after = None
        before = None
        if intermediate_point.is_almost(self.endpoints[0]):
            after = self
        elif intermediate_point.is_almost(self.endpoints[1]):
            before = self
        else:
            before = self.copy()
            after = self.copy()
            before.endpoints[1] = intermediate_point.copy()
            after.endpoints[0] = intermediate_point
        return (before, after)

    def is_almost_vertical(self):
        """
        are endpoints almost aligned vertically ?
        example:
            segment = Segment([Point([3, 3]), Point([3, 5])])
            segment.is_almost_vertical() # is True
            segment = Segment([Point([3, 3]), Point([2, 5])])
            segment.is_almost_vertical() # is False
        """
        return is_almost(*[p.get_x() for p in self.endpoints])

    def is_almost_horizontal(self):
        """
        are endpoints almost aligned horizontally ?
        """
        return is_almost(*[p.get_y() for p in self.endpoints])

    def lowest_endpoint(self):
        """
        return one of lowest endpoints (y maximized).

        example:
            segment = Segment([Point([5, 3]), Point([8, 1])])
            low_point = segment.lowest_endpoint()
            # low_point is (5,3)
        """
        y_1, y_2 = [p.get_y() for p in self.endpoints]
        if y_1 > y_2:
            return self.endpoints[0]
        else:
            return self.endpoints[1]

    def slope(self):
        """
        return slope of line going through endpoints.
        """
        (x_1, y_1), (x_2, y_2) = [p.coordinates for p in self.endpoints]
        if is_almost(x_1, x_2):
            if y_2 > y_1:
                return float("inf")
            else:
                return float("-inf")
        return (y_2 - y_1)/(x_2 - x_1)

    def __lt__(self, other):
        return self.comparer.key(self) < self.comparer.key(other)

    def __le__(self, other):
        return self.comparer.key(self) <= self.comparer.key(other)

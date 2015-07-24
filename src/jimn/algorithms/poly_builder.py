# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.polygon import polygon
from jimn.segment import segment
from jimn.displayable import tycat, tycat_set_svg_dimensions
from jimn.utils.debug import is_module_debugged

area_limit = 10**-10 # TODO: what is the right value ??

class polygon_builder:
    def __init__(self, segments):
        # initialize various structs to hold info
        self.points = []
        self.polygons = []
        self.previous_point = None
        self.current_point = None

        # start
        self.segments = segments
        self.marked_segments = {}  # we go only once through each (oriented) segment

        # compute structures to easily enter and leave points
        # we need to quickly find neighbours for any point
        self.hash_points()
        self.sort_neighbours_by_angle()

    def tycat(self):
        tycat(self.polygons, self.points, self.previous_point, self.current_point, self.segments, list(self.marked_segments.values()))

    # compute for each point the list of neighbouring points
    def hash_points(self):
        self.points_neighbours = {}
        for s in self.segments:
            endpoints, reversed_endpoints = s.get_endpoints(), reversed(s.get_endpoints())
            for (p1, p2) in (endpoints, reversed_endpoints):
                if p1 in self.points_neighbours:
                    self.points_neighbours[p1].append(p2)
                else:
                    self.points_neighbours[p1] = [p2]

    # sort all neighbours so that we can easily turn around a point
    def sort_neighbours_by_angle(self):
        for point, neighbours in self.points_neighbours.items():
            sorted_neighbours = sorted(neighbours, key=lambda neighbour: point.angle_with(neighbour), reverse=True)
            self.points_neighbours[point] = sorted_neighbours

    def build_polygons(self):
        for s in self.segments:
            if s in self.marked_segments:
                continue  # skip segments already used
            try:
                p = self.build_polygon(s)
            except:
                tycat_set_svg_dimensions(1024, 768)
                print(*self.points)
                self.polygons = []
                self.tycat()
                raise

            if p is not None and p.is_oriented_clockwise():  # discard outer edge
                self.polygons.append(p)
                if __debug__:
                    if is_module_debugged(__name__):
                        print("added polygon")
                        self.tycat()
        return self.polygons

    def find_next_point(self, current_point, previous_point):
        neighbours = self.points_neighbours[current_point]
        length = len(neighbours)
        index = neighbours.index(previous_point)
        next_index = (index+1) % length
        next_point = neighbours[next_index]
        return next_point

    def build_polygon(self, start_segment):
        self.points = []

        self.start_point, self.current_point = start_segment.get_endpoints()
        self.points.append(self.start_point)
        self.marked_segments[start_segment] = start_segment

        self.previous_point = self.start_point

        while self.current_point != self.start_point:
            self.points.append(self.current_point)

            # continue moving
            next_point = self.find_next_point(self.current_point, self.previous_point)
            self.previous_point = self.current_point
            self.current_point = next_point
            s = segment([self.previous_point, self.current_point])
            self.marked_segments[s] = s

        p = polygon(self.points)
        if abs(p.area()) > area_limit:
            p.remove_useless_points()
            return p
        else:  # discard polygons which are too small
            return None


def build_polygons(segments):
    """
    turns a set of segments into a set of polygons.
    works by following edges.
    pre-requisite: no segments intersect other than at endpoints.
    """
    builder = polygon_builder(segments)
    return builder.build_polygons()

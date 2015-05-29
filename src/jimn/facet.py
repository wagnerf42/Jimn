# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.point import point
from jimn.segment import segment


class facet:
    def __init__(self, points):
        self.points = points

    def __str__(self):
        return "[{}]".format(';'.join(map(lambda p: str(p), self.points)))

    def segments(self):
        p1, p2, p3 = self.points
        return [segment([p1, p2]), segment([p1, p3]), segment([p2, p3])]

    def add_point(self, p):
        self.points.append(p)

    def find_points_above_and_below(self, h):
        points = [[], []]
        for p in self.points:
            points[p.is_above(h)].append(p)
        return points

    def intersect(self, h):
        lower_points, higher_points = self.find_points_above_and_below(h)
        if len(higher_points) == 3:  # are we reused later
            kept_facet = None
        else:
            kept_facet = self
        if len(lower_points) == 2:
            together_points = lower_points
            isolated_point = higher_points[0]
        elif len(higher_points) == 2:
            together_points = higher_points
            isolated_point = lower_points[0]
            if(isolated_point.get_z() == h):
                return (kept_facet, None)
        else:
            return (kept_facet, None)

        traversing_segments = [segment([p, isolated_point]) for p in together_points]
        intersection_points = [s.intersect(h) for s in traversing_segments]

        intersection_segment = segment(intersection_points)
        return (kept_facet, intersection_segment)


def binary_facet(all_coordinates):
    f = facet(
        [
            point(all_coordinates[3:6]),
            point(all_coordinates[6:9]),
            point(all_coordinates[9:12])
        ]
    )
    heights = (all_coordinates[5], all_coordinates[8], all_coordinates[11])
    max_height = max(heights)
    min_height = min(heights)
    return (f, min_height, max_height)

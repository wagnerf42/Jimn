"""
associate to each path of the envelope the corresponding part on original
object generating the envelope.
"""
from jimn.point import Point
from jimn.segment import Segment
from jimn.arc import Arc
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.coordinates_hash import ROUNDER2D


class DisplacedPath:
    """
    a displaced path is an elementary path (or point) on an envelope associated
    to the original content which generated it.
    """
    def __init__(self, envelope_path, origin):
        self.path = envelope_path
        self.origin = origin

    def get_endpoint(self, index):
        """
        get endpoint 0 or 1 on envelope 'path'.
        needed because this path could be just a point.
        """
        if isinstance(self.path, Point):
            return self.path
        else:
            return self.path.endpoints[index]

    @classmethod
    def displace(cls, path, distance):
        """
        displace given path by given distance on outer side.
        """
        if isinstance(path, Segment):
            parallel_segment = path.parallel_segment(distance, -1)
            hashed_segment = Segment([ROUNDER2D.hash_point(p)
                                      for p in parallel_segment.endpoints])
            return cls(hashed_segment, path)
        else:
            if path.reversed_direction:
                new_radius = 2 * path.radius
                new_points = [p*2-path.center for p in path.endpoints]
                hashed_points = [ROUNDER2D.hash_point(p) for p in new_points]
                inflated_path = Arc(new_radius, hashed_points, path.center, True)
            else:
                inflated_path = ROUNDER2D.hash_point(path.center)
            return cls(inflated_path, path)

    def project(self, point):
        """
        find where point on stored path projects itself on origin.
        """
        if isinstance(self.origin, Point):
            result = self.origin
        elif isinstance(self.origin, Segment):
            result = self.origin.point_projection(point)
        else:
            intersections = self.origin.intersections_with_segment(
                Segment([self.origin.center, point])
            )
            assert len(intersections) == 1
            result = intersections[0]

        if __debug__:
            if is_module_debugged(__name__):
                print("project from envelope back to original path")
                tycat(self.path, self.origin, point, result)

        return result

    def interferences_with(self, other):
        """
        if paths overlap return beginning and end points of overlapping
        section.
        if not and if they intersect, return intersection points
        """
        interferences = _overlapping_points(self.path, other.path)
        interferences.extend(
            self.path.intersections_with(other.path))
        return interferences

    def reconnect(self, next_path, distance):
        """
        connect self with next.
        return self and path needed to go to next.
        if self is just a point, skip self.
        """
        current_point = self.get_endpoint(1)
        next_point = next_path.get_endpoint(0)
        connected_paths = []
        if not isinstance(self.path, Point):
            connected_paths.append(self)

        if current_point != next_point:
            center = self.origin.endpoints[1]
            displaced_path = DisplacedPath(
                Arc(distance, [current_point, next_point], center, True), center
            )
            connected_paths.append(displaced_path)
        return connected_paths


def _overlapping_points(followed, other):
    """
    if followed and other overlap returns start and end points
    of overlapping sections.
    NOTE : - some points might be duplicated
           - will also return included endpoints
    """
    return [
        ep
        for p, q in zip((followed, other), (other, followed))
        for ep in q.endpoints
        if p.contains(ep)
    ]

"""
associate to each path of the envelope the corresponding part on original
object generating the envelope.
"""
from jimn.point import Point
from jimn.segment import Segment
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged


class DisplacedPath:
    """
    a displaced path is an elementary path on an envelope associated
    to the original content which generated it.
    """
    def __init__(self, envelope_path, origin):
        self.path = envelope_path
        self.origin = origin

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

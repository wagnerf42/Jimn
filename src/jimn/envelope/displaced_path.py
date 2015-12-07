"""
associate to each path of the envelope the corresponding part on original
object generating the envelope.
"""


class displaced_path:
    def __init__(self, envelope_path, origin):
        self.path = envelope_path
        self.origin = origin

    def project(self, p):
        """
        find where p on stored path projects itself on origin
        """
        if isinstance(self.origin, point):
            result = self.origin
        elif isinstance(self.origin, segment):
            result = self.origin.point_projection(p)
        else:
            raise Exception("TODO: arcs")

        if __debug__:
            if is_module_debugged(__name__):
                print("project from envelope back to original path")
                tycat(self.path, self.origin, p, result)

        return result

    def interferences_with(self, other):
        """
        if paths overlap return beginning and end points of overlapping
        section.
        if not and if they intersect, return intersection points
        """
        interferences = _overlapping_points(self.path, other.path)
        interferences.extend(
            self.path.intersections_with(other.path, rounder2d))
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
        for ep in q.get_endpoints()
        if p.contains(ep)
    ]

from jimn.utils.coordinates_hash import rounder2d
from jimn.point import point
from jimn.segment import segment
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged

from jimn.bounding_box import bounding_box
from jimn.arc import arc
from jimn.segment import segment
from jimn.pocket import pocket
from jimn.displayable import tycat
from jimn.utils.coordinates_hash import rounder2d
from jimn.utils.debug import is_module_debugged
from jimn.envelope.displaced_path import displaced_path

"""
envelope contains the edge of a pocket (no holes) and its limit surroundings
at given distance.
it allows to quickly find for any given position at limit which position
in paths it corresponds to
"""


class envelope:
    def __init__(self, inside_content, distance):
        """
        inflates path by given distance
        """
        self.distance = distance
        self.inside_content = inside_content  # for debug only

        if isinstance(inside_content, pocket):
            self._fill_from_pocket(inside_content)
        else:
            if isinstance(inside_content, arc):
                self._fill_from_arc(inside_content)
            else:
                self._fill_from_segment(inside_content)

        if __debug__:
            if is_module_debugged(__name__):
                print("inflating")
                tycat(self)

    def get_bounding_box(self):
        box = bounding_box.empty_box(2)
        for p in self.paths:
            box.update(p.path.get_bounding_box())
        return box

    def save_svg_content(self, display, color):
        for p in self.paths:
            p.path.save_svg_content(display, color)
        self.inside_content.save_svg_content(display, color)

    def _fill_from_segment(self, s):
        """
        creates envelope by inflating segment
        """
        sides = [
            s.parallel_segment(self.distance, rounder2d, side)
            for side in (-1, 1)
        ]
        # TODO: I think we don't need the arcs
        sides.append(
            arc(
                self.distance,
                [sides[0].get_endpoint(0), sides[1].get_endpoint(0)],
                s.get_endpoint(0)
            )
        )
        sides.append(
            arc(
                self.distance,
                [sides[1].get_endpoint(1), sides[0].get_endpoint(1)],
                s.get_endpoint(1)
            )
        )
        self.paths = [
            displaced_path(a, b)
            for a, b in zip(sides, [s, s, s.get_endpoint(0), s.get_endpoint(1)])
        ]

    def _fill_from_arc(self, a):
        """
        creates envelope by inflating arc
        """
        # get endpoints
        # we need them in non-reversed order for inflating
        # (inflation does not depend on direction)
        p2, p4 = a.get_stored_endpoints()
        p3 = a.get_center()
        p1 = p2 + p2 - p3
        p5 = p4 + p4 - p3
        a1 = arc(self.distance, (p3, p1), p2)
        a2 = arc(self.distance, (p5, p3), p4)
        a3 = arc(2*self.distance, (p1, p5), p3)
        # TODO: I don't think we need a1 and a2 here
        self.paths = [
            displaced_path(p, o) for p, o in zip([a1, a2, a3], [p2, p4, a])
        ]

    def _fill_from_pocket(self, inside_pocket):
        """
        creates envelope by inflating pocket
        """
        self.paths = []
        # just follow path, ignoring arcs, moving away segments
        # and reconnecting everything.
        # we need to remember previous segment to do the reconnecting
        last_path = inside_pocket.paths[-1]
        if isinstance(last_path, segment):
            previous_segment = \
                last_path.parallel_segment(self.distance, rounder2d, -1)
        else:
            previous_segment = None

        for p in inside_pocket.paths:
            if isinstance(p, segment):
                current_segment = (
                    p.parallel_segment(self.distance, rounder2d, -1))
                if previous_segment is not None:
                    center = p.get_endpoint(0)
                    dp = displaced_path(
                        arc(
                            self.distance,
                            [
                                current_segment.get_endpoint(0),
                                previous_segment.get_endpoint(1)
                            ],
                            center,
                            True
                        ),
                        center
                    )
                    self.paths.append(dp)

                self.paths.append(displaced_path(current_segment, p))
                previous_segment = current_segment
            else:
                previous_segment = None

    def junction_points(self, inner_envelope):
        """
        return couple of points interfering.
        first one on outer envelope, other on inner enveloppe.
        first one is furthest possible interference point in outer enveloppe
        """
        points_couples = []
        for p in self.paths:
            for q in inner_envelope.paths:
                interferences = p.interferences_with(q)
                if __debug__:
                    if is_module_debugged(__name__) and interferences:
                        print("interferences")
                        tycat(self, inner_envelope,
                              p.path, q.path, interferences)

                for i in interferences:
                    points_couples.append((p.project(i), q.project(i)))

        if not points_couples:
            return None, None

        if self.inside_content.is_sorted():
            last_couple = max(points_couples, key=lambda c: c[0])
        else:
            last_couple = min(points_couples, key=lambda c: c[0])

        return last_couple

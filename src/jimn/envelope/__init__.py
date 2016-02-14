"""
envelope contains the edge of a pocket (no holes) and its limit surroundings
at given distance.
it allows to quickly find for any given position at limit which position
in paths it corresponds to
"""

from jimn.bounding_box import BoundingBox
from jimn.arc import Arc
from jimn.pocket import Pocket
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.iterators import all_two_elements
from jimn.envelope.displaced_path import DisplacedPath
from jimn.caching import cached


class Envelope:
    """
    region of spaced reached around a given path set with a circular mill
    """
    def __init__(self, inside_content, distance):
        """
        inflates path by given distance
        """
        self.distance = distance
        self.inside_content = inside_content  # for debug only

        if isinstance(inside_content, Pocket):
            try:
                self._fill_from_pocket(inside_content)
            except:
                print("failed compute envelope for", self.inside_content)
                raise
        else:
            if isinstance(inside_content, Arc):
                self._fill_from_arc(inside_content)
            else:
                self._fill_from_segment(inside_content)

        if __debug__:
            if not self.paths:
                print("cannot create envelope out of", self.inside_content)
                tycat(self.inside_content)

        if __debug__:
            if is_module_debugged(__name__):
                print("inflating")
                tycat(self)

    @cached
    def get_bounding_box(self):
        """
        smallest bounding box containing envelope
        """
        box = BoundingBox.empty_box(2)
        for displaced_path in self.paths:
            box.update(displaced_path.path.get_bounding_box())
        return box

    def get_display_string(self, display, color):
        """
        return svg string used for displaying envelope in final display.
        there is less info than in the standard tycat and returning
        the string instead of directly displaying it allows for caching.
        """

        first_point = self.paths[0].path.endpoints[0]
        first_coordinates = display.convert_coordinates(first_point.coordinates)
        initial_move = "M {},{}".format(*first_coordinates)

        head = "<path d=\""
        paths_strings = [p.path.get_display_string(display) for p in self.paths]
        foot = "\" fill=\"" + color + "\" stroke=\"none\"/>\n"

        return head + initial_move + " ".join(paths_strings) + foot

    def save_svg_content(self, display, color):
        """
        svg for tycat
        """
        for displaced_path in self.paths:
            displaced_path.path.save_svg_content(display, color)
        self.inside_content.save_svg_content(display, color)

    def _fill_from_segment(self, segment):
        """
        creates envelope by inflating segment
        """
        sides = []
        sides.append(segment.parallel_segment(self.distance, -1))
        sides.append(segment.parallel_segment(self.distance, 1).reverse())
        # TODO: I think we don't need the arcs
        point_1, point_2 = segment.endpoints
        arcs = []
        arcs.append(
            Arc(
                self.distance,
                [sides[0].endpoints[1], sides[1].endpoints[0]],
                point_1,
                reversed_direction=True
            )
        )
        arcs.append(
            Arc(
                self.distance,
                [sides[1].endpoints[1], sides[0].endpoints[0]],
                point_2,
                reversed_direction=True
            )
        )
        self.paths = [
            DisplacedPath(a, b)
            for a, b in zip([arcs[0], sides[1], arcs[1], sides[0]],
                            [point_1, segment, point_2, segment])
        ]

    def _fill_from_arc(self, arc):
        """
        creates envelope by inflating arc
        """
        # get endpoints
        # TODO: I don't think we need the side arcs
        if arc.reversed_direction:
            arc_point_2, arc_point_1 = arc.endpoints
        else:
            arc_point_1, arc_point_2 = arc.endpoints

        displaced_point_1 = arc_point_1 * 2 - arc.center
        displaced_point_2 = arc_point_2 * 2 - arc.center
        side_arc_point_1 = Arc(self.distance,
                               (arc.center, displaced_point_1),
                               arc_point_1)
        side_arc_point_2 = Arc(self.distance,
                               (displaced_point_2, arc.center),
                               arc_point_2)
        displaced_main_arc = Arc(2*self.distance,
                                 (displaced_point_1, displaced_point_2),
                                 arc.center)
        self.paths = [
            DisplacedPath(p, o)
            for p, o in zip(
                [side_arc_point_1, displaced_main_arc, side_arc_point_2],
                [arc_point_1, arc, arc_point_2]
            )
        ]

    def _fill_from_pocket(self, inside_pocket):
        """
        creates envelope by inflating pocket
        """
        self.paths = []
        # just follow path, moving away
        raw_paths = [DisplacedPath.displace(p, self.distance)
                     for p in inside_pocket.paths]

        # and then reconnecting everything.
        for previous_path, current_path in all_two_elements(raw_paths):
            self.paths.extend(previous_path.reconnect(current_path,
                                                      self.distance))

    def junction_points(self, inner_envelope):
        """
        return couple of points interfering.
        first one on outer envelope, other on inner enveloppe.
        first one is furthest possible interference point in outer enveloppe
        """
        points_couples = []
        for our_path in self.paths:
            for envelope_path in inner_envelope.paths:
                interferences = our_path.interferences_with(envelope_path)
                if __debug__:
                    if is_module_debugged(__name__) and interferences:
                        print("interferences")
                        tycat(self, inner_envelope,
                              our_path.path, envelope_path.path, interferences)

                for i in interferences:
                    points_couples.append((our_path.project(i),
                                           envelope_path.project(i)))

        if not points_couples:
            return None, None

        if self.inside_content.endpoints[0] < self.inside_content.endpoints[1]:
            last_couple = max(points_couples, key=lambda c: c[0])
        else:
            last_couple = min(points_couples, key=lambda c: c[0])

        return last_couple

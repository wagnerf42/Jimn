"""
envelope contains the edge of a pocket (no holes) and its limit surroundings
at given distance.
it allows to quickly find for any given position at limit which position
in paths it corresponds to
"""

from jimn.bounding_box import BoundingBox
from jimn.pocket import Pocket
from jimn.displayable import tycat
from jimn.utils.debug import is_module_debugged
from jimn.utils.iterators import all_two_elements
from jimn.envelope.displaced_path import DisplacedPath
from jimn.utils.caching import cached


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
            inside = inside_content.paths
        else:
            inside = (inside_content, inside_content.reverse())

        try:
            self.paths = []
            # just follow path, moving away
            raw_paths = [DisplacedPath.displace(p, distance) for p in inside]

            # and then reconnecting everything.
            for path1, path2 in all_two_elements(raw_paths):
                self.paths.extend(path1.reconnect(path2, distance))

        except:
            print("failed compute envelope for", self.inside_content)
            tycat(self.inside_content, [p.path for p in raw_paths])
            raise

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

    def svg_content(self):
        """
        svg for tycat
        """
        string = "".join([p.svg_content() for p in self.paths])
        string += self.inside_content.svg_content()

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

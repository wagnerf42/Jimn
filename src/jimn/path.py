"""
a path is a list of arcs, segments or vertical paths
"""
import os
from copy import copy
from math import floor
from collections import defaultdict
from jimn.displayable import tycat
from jimn.utils.coordinates_hash import CoordinatesHash
from jimn.vertical_path import VerticalPath
from jimn.path_position import PathPosition
from jimn.bounding_box import BoundingBox
from jimn.point import Point
from jimn.envelope import Envelope
from jimn.displayable import tycat_start, tycat_end


PATH_IMAGES = os.environ.get("JIMN_PATH_ANIMATION")
if PATH_IMAGES is None:
    PATH_IMAGES = 10
else:
    PATH_IMAGES = int(PATH_IMAGES)


class Path:
    """
    a path is a list of arcs, segments or vertical paths
    """
    def __init__(self, elementary_paths):
        self.elementary_paths = elementary_paths

    def get_first_point(self):
        """
        return first point in path
        """
        return self.elementary_paths[0].endpoints[0]

    def translate(self, translation):
        """
        translates the whole path by a given translation vector.
        returns new path if obtained path is different and same path
        if translation vector is (0,0)
        """
        if translation.is_almost(Point([0, 0])):
            return self
        return Path([p.translate(translation) for p in self.elementary_paths])

    def set_elementary_paths(self, elementary_paths):
        """
        replace elementary paths in self by given paths
        """
        self.elementary_paths = elementary_paths

    def get_bounding_box(self):
        """
        min bounding box for whole path
        """
        box = BoundingBox.empty_box(2)
        for path in self.elementary_paths:
            box.update(path.get_bounding_box())
        return box

    def save_svg_content(self, display, color):
        """
        svg for tycat
        """
        self.get_first_point().save_svg_content(display, color)
        horizontal_paths = self.hash_horizontal_paths_by_height()
        count = 0
        for height in sorted(list(horizontal_paths.keys()), reverse=True):
            paths = horizontal_paths[height]
            new_color = display.svg_color_after(color, count)
            for path in paths:
                path.save_svg_content(display, new_color)
            count += 1

    def hash_horizontal_paths_by_height(self):
        """
        hash each horizontal path to its corresponding height
        """
        horizontal_paths = defaultdict(list)
        current_height = 0
        for path in self.elementary_paths:
            if isinstance(path, VerticalPath):
                current_height = path.update_height(current_height)
            else:
                horizontal_paths[current_height].append(path)
        return horizontal_paths

    def length(self):
        """
        return total length of path
        """
        return sum([p.length() for p in self.elementary_paths])

    def append(self, elementary_path):
        """
        add elementary path at end of path
        """
        self.elementary_paths.append(elementary_path)

    def extend(self, paths):
        """
        add elementary paths at end of path
        """
        self.elementary_paths.extend(paths)

    def animate(self, milling_radius):
        """
        step by step animation for carving the path with
        given milling radius.
        """
        total_length = self.length()
        steps_length = total_length / PATH_IMAGES

        svg_paths_strings = dict()  # strings for seen paths
        seen_envelopes = []  # all paths moved upon and seen
        unseen_envelopes = []  # all paths moved upon but unseen
        bounding_box = BoundingBox.empty_box(2)  # their boxes
        last_used_box = BoundingBox.empty_box(2)  # last box used in display
        # if box does not change between two displays, we can keep
        # all generated strings

        current_height = 0
        current_length = 0
        heights_hash = CoordinatesHash(1)

        for path in self.elementary_paths:
            new_length = current_length + path.length()

            if isinstance(path, VerticalPath):
                current_height = path.update_height(current_height)
                current_height = heights_hash.hash_coordinate(current_height)
            else:
                envelope = Envelope(path, milling_radius)
                tycat(envelope)
                unseen_envelopes.append((envelope, current_height))
                bounding_box.update(envelope.get_bounding_box())

            if floor(current_length / steps_length) != \
                    floor(new_length / steps_length):
                _display_animation(last_used_box, bounding_box,
                                   seen_envelopes, unseen_envelopes,
                                   svg_paths_strings)
                last_used_box = copy(bounding_box)

            current_length = new_length

    def find_position(self, point):
        """
        find first position of given point on path.
        (see Path_Position class)
        """
        for i, path in enumerate(self.elementary_paths):
            if path.contains(point):
                return PathPosition(path, point, i)

        raise Exception("point not found in path")

    def change_starting_point(self, position):
        """
        change starting point of cycle path.
        takes a position as argument.
        """
        start = self.elementary_paths[:position.index]
        end = self.elementary_paths[position.index+1:]
        new_cycle = []
        start_path = self.elementary_paths[position.index]
        before, after = start_path.split_around(position.point)

        if after is not None:
            new_cycle.append(after)
        new_cycle.extend(end)
        new_cycle.extend(start)
        if before is not None:
            new_cycle.append(before)
        self.elementary_paths = new_cycle

    def get_dot_label(self):
        """
        label for displaying trees containing paths.
        (see Path_Tree class)
        """
        return str(id(self))

    def get_points(self):
        """
        iterates through all points.
        """
        for path in self.elementary_paths:
            yield path.endpoints[0]

    def __hash__(self):
        return hash(id(self))

    def __str__(self):
        path_strings = [str(p) for p in self.elementary_paths]
        return "Path([\n    " + ",\n    ".join(path_strings) + "\n])"


def _display_animation(last_used_box, bounding_box,
                       seen_envelopes, unseen_envelopes,
                       svg_paths_strings):
    """
    one step of animation process.
    strings are pre-computed to avoid re-computations for each frame.
    we only recompute strings when forced : when bounding box changes.
    """

    # invalidate strings cache if needed
    if last_used_box != bounding_box:
        seen_envelopes.extend(unseen_envelopes)
        unseen_envelopes = seen_envelopes
        seen_envelopes = []
        svg_paths_strings = dict()

    # compute missing strings
    display = tycat_start(None, bounding_box)
    for envelope, height in unseen_envelopes:
        if height not in svg_paths_strings:
            svg_paths_strings[height] = "<path d=\""
        svg_paths_strings[height] += envelope.get_display_string(display)

    # save svg
    color_index = 1
    for height in sorted(list(svg_paths_strings.keys()), reverse=True):
        color = display.svg_color(color_index)
        color_index += 1
        display.write(svg_paths_strings[height])
        display.write("\" fill=\"" + color + "\" stroke=\"none\"/>\n")

    tycat_end(display)

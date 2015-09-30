
class path_position:
    def __init__(self, p, ep, index):
        """
        creates an object storing a position on a path.
        records:
            - the point
            - the elementary path on which the point is
            - the index of the elementary path in the path
        """
        self.p = p
        self.ep = ep
        if index >= 0:
            self.distance = ep.squared_distance_from_start(p)
        self.index = index

    @classmethod
    def empty_position(cls):
        """
        returns a non existing position which will always compare as
        less than a real one
        """
        return cls(None, None, -1)

    def is_not_empty(self):
        """
        returns true if position really contains something
        """
        return self.index >= 0

    def __lt__(self, other):
        """
        compares to position on same path.
        true if self is reached before other when following path from its start
        """
        if self.index < other.index:
            return True
        if self.index > other.index:
            return False
        return self.distance < other.distance


def inflate_segment(s, radius):
    """
    returns pocket around segment reachable by given radius
    """
    rounder = coordinates_hash(2)
    sides = [
        s.parallel_segment(radius, rounder, side) for side in (-1, 1)
    ]
    sides.append(
        arc(
            radius,
            [sides[0].get_endpoint(0), sides[1].get_endpoint(0)],
            s.get_endpoint(0)
        )
    )
    sides.append(
        arc(
            radius,
            [sides[1].get_endpoint(1), sides[0].get_endpoint(1)],
            s.get_endpoint(1)
        )
    )
    return pocket(sides)


def inflate_arc(a, radius):
    """
    returns pocket around arc reachable by given radius
    """
    assert radius == a.get_radius()
    p2, p4 = a.get_endpoints()
    p3 = a.get_center()
    diff = (p2-p3)
    p1 = p2 + diff
    p5 = p4 - diff
    a1 = arc(radius, (p3, p1), p2)
    a2 = arc(radius, (p5, p3), p4)
    a3 = arc(2*radius, (p1, p5), p3)
    return pocket([a1, a2, a3])


def overlapping_area_exit_point(followed, other, radius, index):
    """
    when we advance on 'followed' path with a drill of given radius
    we might interfere with the drill of 'other'.
    if interference stops before end of followed,
    return last position of interference
    """
    # if distance from end to other is < radius return immediately
    end_distance = other.distance_to_point(followed.get_endpoint(1))
    if is_almost(end_distance, radius) or end_distance < radius:
        return
    # compute inflation of followed and other
    inflated_followed = followed.inflate(radius)
    inflated_other = other.inflate(radius)

    # if bounding boxes do not intersect, leave immediately
    b1 = inflated_followed.get_bounding_box()
    b2 = inflated_other.get_bounding_box()

    if not b1.intersects(b2):
        return

    # compute intersection points
    intersections = inflated_followed.intersects(inflated_other)
    # if no intersection return
    if len(intersections) == 0:
        return
    # compute points on followed reaching all these intersections
    on_path_points = []
    for p in intersections:
        on_path_points.extend(followed.points_at_distance(p, radius))
    if __debug__:
        if is_module_debugged(__name__):
            tycat(followed, inflated_followed, inflated_other,
                  intersections, on_path_points)
    # find path point nearest from followed.p2
    d = followed.endpoints[1] - followed.endpoints[0]
    last_point = max(
        on_path_points, key=lambda p: followed.squared_distance_from_start(p)
    )
    return path_position(last_point, followed, index)


def overlap_exit_position(outer_path, inner_path, milling_radius):
    """
    imagine we are following outer path.
    at some point we can guarantee that we will never interfere again
    with inner path.
    returns a marker for this position.
    """
    outer_paths = outer_path.get_elementary_paths()
    inner_paths = inner_path.get_elementary_paths()
    # we start from end of outer path because we are interested in last
    # place of overlapping
    for outer_index in reversed(range(len(outer_paths))):
        position = path_position.empty_position()
        out = outer_paths[outer_index]
        # try overlap with all inner paths one by one
        # TODO: slow -> surely better algorithms are possible
        for p in inner_paths:
            new_position = overlapping_area_exit_point(
                out, p, milling_radius, outer_index
            )
            if new_position:
                if position < new_position:
                    position = new_position
        if position.is_not_empty():
            return position
    raise Exception("no path intersection")


def merge_path(outer_path, inner_path, p):
    assert False, "TODO"


from jimn.arc import arc
from jimn.displayable import tycat
from jimn.pocket import pocket
from jimn.utils.coordinates_hash import coordinates_hash
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import is_almost

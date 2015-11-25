class dual_position:
    def __init__(self, outer_point, inner_point,
                 outer_path, inner_path,
                 outer_index, inner_index):
        """
        stores positions on two different paths.
        used when switching between different paths.
        """
        self.outer_position = path_position(outer_point, outer_path,
                                            outer_index)
        self.inner_position = path_position(inner_point, inner_path,
                                            inner_index)

    def __lt__(self, other):
        """
        true if self's outer position is closer from start of path
        than other's outer position.
        both outer positions need to be on same outer path.
        """
        return self.outer_position < other.outer_position


def inflate_segment(s, radius):
    """
    returns pocket around segment reachable by given radius
    """
    sides = [
        s.parallel_segment(radius, rounder2d, side) for side in (-1, 1)
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
    # get endpoints
    # we need them in non-reversed order for inflating
    # (inflation does not depend on direction)
    p2, p4 = a.get_stored_endpoints()
    p3 = a.get_center()
    p1 = p2 + p2 - p3
    p5 = p4 + p4 - p3
    a1 = arc(radius, (p3, p1), p2)
    a2 = arc(radius, (p5, p3), p4)
    a3 = arc(2*radius, (p1, p5), p3)
    return pocket([a1, a2, a3])


def segments_might_overlap(s1, s2):
    """
    returns if segments are aligned
    """
    return s1.line_hash(rounder_lines) == s2.line_hash(rounder_lines)


def arcs_might_overlap(a1, a2):
    """
    returns if arcs follow same circle
    """
    same_centers = a1.get_center().is_almost(a2.get_center())
    same_radii = is_almost(a1.get_radius(), a2.get_radius())
    return same_centers and same_radii


def last_arc_overlapping_point(followed, other):
    """
    if arcs followed and other overlap returns the last point
    of followed overlapping other.
    else returns None.
    overlapping means here overlapping on more than one point
    precondition: arcs have same center and radius
    """
    if followed == other:
        return followed.get_endpoint(1)
    raise Exception("TODO")


def last_segment_overlapping_point(followed, other):
    """
    if segments followed and other overlap returns the last point
    of followed overlapping other.
    else returns None.
    overlapping means here overlapping on more than one point.
    precondition : segments are aligned
    """
    # sort all points following same direction as followed
    # also remember to which segment each point belongs
    start = followed.get_endpoint(0)
    named_points = [(p, 0) for p in followed.get_endpoints()]
    named_points.extend([(p, 1) for p in other.get_endpoints()])

    points = followed.get_endpoints() + other.get_endpoints()
    if followed.get_endpoint(0) > followed.get_endpoint(1):
        start = max(points)
    else:
        start = min(points)

    named_points = sorted(named_points,
                          key=lambda t: (start.squared_distance_to(t[0]))
                          )

    # now, move on points updating state (inside what we are)
    inside = [False, False]
    for p in named_points:
        was_inside = sum(1 for i in inside if i)
        side = p[1]
        inside[side] = not inside[side]
        is_inside = sum(1 for i in inside if i)
        # when we were inside both but exit one
        # we are on wanted point
        if was_inside == 2 and is_inside == 1:
            return p[0]


def last_overlapping_point(followed, other):
    """
    if followed and other overlap returns the last point
    of followed overlapping other.
    else returns None.
    overlapping means here overlapping on more than one point
    """
    if str(type(followed)) == "<class 'jimn.arc.arc'>" and \
            str(type(other)) == "<class 'jimn.arc.arc'>":
        if arcs_might_overlap(followed, other):
            return last_arc_overlapping_point(followed, other)
    elif str(type(followed)) != "<class 'jimn.arc.arc'>" and \
            str(type(other)) != "<class 'jimn.arc.arc'>":
        if segments_might_overlap(followed, other):
            return last_segment_overlapping_point(followed, other)


def last_points_reaching(followed, other, intersections, radius):
    """
    given two elementary paths followed and other
    and a set of intersections of the pockets of size radius around them
    return the last point of followed at distance radius of an intersection
    together with a point at same distance of same intersection in other
    """
    # compute points on followed and others reaching all these intersections
    on_path_points = []
    for p in intersections:
        outer_points = followed.points_at_distance(p, radius)
        inner_points = other.points_at_distance(p, radius)
        try:
            inner_point = inner_points[0]  # we can keep any of inner points
        except:
            print("failed", followed, other)
            circles = [circle(i, radius) for i in intersections]
            tycat(followed, other, intersections, circles)
            raise
        for q in outer_points:
            on_path_points.append([q, inner_point])
    # find path point nearest from followed.p2 and corresponding point in other
    return max(
        on_path_points, key=lambda p: followed.squared_distance_from_start(p[0])
    )


def overlapping_area_exit_point(followed, other, radius,
                                outer_index, inner_index):
    """
    when we advance on 'followed' path with a drill of given radius
    we might interfere with the drill of 'other'.
    if interference stops before end of followed,
    return last position of interference
    """
    interference_point = last_overlapping_point(followed, other)
    if interference_point:
        return dual_position(
            interference_point, interference_point,
            followed, other,
            outer_index, inner_index
        )

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

    try:
        outer_point, inner_point = last_points_reaching(
            followed, other, intersections, radius
        )
    except:
        print("we cannot find last points for")
        tycat(followed, other, inflated_followed, inflated_other, intersections)
        raise

    return dual_position(outer_point, inner_point,
                         followed, other,
                         outer_index, inner_index)


def overlap_exit_pocket_position(outer_path, inner_pocket, milling_radius):
    """
    imagine we are following outer path.
    at some point we can guarantee that we will never interfere again
    with inner pocket.
    returns a marker for this position.
    """
    outer_paths = outer_path.get_elementary_paths()
    inner_paths = inner_pocket.get_content()
    # we start from end of outer path because we are interested in last
    # place of overlapping
    for outer_index in reversed(range(len(outer_paths))):
        position = None
        out = outer_paths[outer_index]
        # try overlap with all inner paths one by one
        # TODO: slow -> surely better algorithms are possible
        for inner_index, p in enumerate(inner_paths):
            new_position = overlapping_area_exit_point(
                out, p, milling_radius, outer_index, inner_index
            )
            if new_position:
                if position is None:
                    position = new_position
                else:
                    if position < new_position:
                        position = new_position

        if position is not None:
            return position


def update_inner_position(inner_path, position):
    """
    find where inner point is on given inner path and update position.
    """
    position.inner_position = \
        inner_path.find_position(position.inner_position.point)
    return position


def overlap_exit_position(outer_path, inner_path, inner_pocket, milling_radius):
    """
    imagine we are following outer path.
    at some point we can guarantee that we will never interfere again
    with inner path.
    returns a marker for this position.
    knowing the pocket containing the inner path allows for computations
    speed up since exit position belongs to the edge
    """
    pocket_position = overlap_exit_pocket_position(outer_path, inner_pocket,
                                                   milling_radius)
    if __debug__:
        if not pocket_position:
            raise Exception("no path intersection")

    position = update_inner_position(inner_path, pocket_position)

    if __debug__:
        if is_module_debugged(__name__):
            print("found exit point at", position.outer_position.index)
            try:
                s = segment(
                    [
                        position.outer_position.point,
                        position.inner_position.point
                    ]
                )
                tycat(outer_path, inner_path, s)
            except:
                tycat(outer_path, inner_path)

    return position


def merge_path(outer_path, inner_path, position):
    """
    merge inner path inside outer path at given position.
    Note that since positions contains array indices you
    need to merge starting from last path.
    """
    paths = outer_path.get_elementary_paths()
    outer_point = position.outer_position.point
    inner_point = position.inner_position.point
    if __debug__:
        if is_module_debugged(__name__):
            print("merging at", position.outer_position.index)
            tycat(outer_path, inner_path, position.outer_position.ep)
    arrival_path = paths[position.outer_position.index]
    assert arrival_path.contains(outer_point), "no merging here"

    sub_path = []
    before, after = arrival_path.split_around(outer_point)
    if before is not None:
        sub_path.append(before)

    if not outer_point.is_almost(inner_point):
        sub_path.append(segment([outer_point, inner_point]))

    sub_path.append(vertical_path(-1))
    sub_path.extend(inner_path.get_elementary_paths())
    sub_path.append(vertical_path(1))

    if not outer_point.is_almost(inner_point):
        sub_path.append(segment([inner_point, outer_point]))

    if after is not None:
        sub_path.append(after)

    paths[position.outer_position.index:position.outer_position.index] = \
        sub_path
    outer_path.set_elementary_paths(paths)


from jimn.arc import arc
from jimn.displayable import tycat
from jimn.pocket import pocket
from jimn.segment import segment
from jimn.utils.coordinates_hash import rounder2d, rounder_lines
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import is_almost
from jimn.vertical_path import vertical_path
from jimn.circle import circle
from jimn.path_position import path_position

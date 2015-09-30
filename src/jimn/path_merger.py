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


def overlapping_area_exit_point(followed, other, radius):
    """
    when we advance on 'followed' path with a drill of given radius
    we might interfere with the drill of 'other'.
    if interference stops before end of followed,
    return last point of interference
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
        on_path_points, key=lambda p: p.scalar_product(d)
    )
    return last_point


def overlap_interval(outer_path, inner_path, milling_radius):
    assert False, "TODO"


def merge_path(outer_path, inner_path, p):
    assert False, "TODO"


from jimn.arc import arc
from jimn.displayable import tycat
from jimn.pocket import pocket
from jimn.utils.coordinates_hash import coordinates_hash
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import is_almost

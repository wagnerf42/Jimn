from jimn.utils.coordinates_hash import coordinates_hash
from jimn.pocket import pocket
from jimn.arc import arc


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

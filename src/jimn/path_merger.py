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

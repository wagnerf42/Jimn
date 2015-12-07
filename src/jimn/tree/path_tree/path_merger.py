def overlap_exit_pocket_position(outer_path, inner_pocket, milling_radius):
    """
    imagine we are following outer path.
    at some point we can guarantee that we will never interfere again
    with inner pocket.
    returns a marker for this position.
    """
    outer_paths = outer_path.elementary_paths
    inner_envelope = envelope(inner_pocket, milling_radius)
    # we start from end of outer path because we are interested in last
    # place of overlapping
    for outer_index in reversed(range(len(outer_paths))):
        out = outer_paths[outer_index]
        outer_envelope = envelope(out, milling_radius)

        outer_point, inner_point = \
            outer_envelope.junction_points(inner_envelope)

        if outer_point:
            return dual_position(out, outer_point, inner_point, outer_index)


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


from jimn.displayable import tycat
from jimn.segment import segment
from jimn.utils.debug import is_module_debugged
from jimn.vertical_path import vertical_path
from jimn.dual_position import dual_position
from jimn.envelope import envelope

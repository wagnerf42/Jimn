"""
all functions related to merging paths together.
we can detect positions to do the merge.
and do the merge.
"""

from jimn.displayable import tycat
from jimn.segment import Segment
from jimn.utils.debug import is_module_debugged
from jimn.vertical_path import VerticalPath
from jimn.tree.path_tree.dual_position import DualPosition
from jimn.envelope import Envelope


def compute_overlap_positions(outer_path, inner_nodes, milling_radius):
    """
    compute for each inner node the position in outer path (just above it)
    where we will never come back above it.
    for each node set result in entrance field.
    """
    outer_paths = outer_path.elementary_paths
    untested_nodes = dict()
    for node in inner_nodes:
        envelope = Envelope(node.old_pocket, milling_radius)
        box = node.old_pocket.get_bounding_box()
        untested_nodes[id(node)] = (node, box, envelope)

    # we start from end of outer path because we are interested in last
    # place of overlapping
    for outer_index in reversed(range(len(outer_paths))):
        if not untested_nodes:
            return
        out = outer_paths[outer_index]
        outer_box = out.get_bounding_box()
        outer_box.inflate(milling_radius)
        # before doing the real intersections (which is computation heavy)
        # we can first intersect bounding boxes
        nodes_found = []  # delete found nodes after loop ; so remember them
        outer_envelope = None
        for node, box, envelope in untested_nodes.values():
            if box.intersects(outer_box):
                if outer_envelope is None:
                    outer_envelope = Envelope(out, milling_radius)
                outer_point, inner_point = \
                    outer_envelope.junction_points(envelope)
                if outer_point:
                    node.entrance = DualPosition(out, outer_point, outer_index)
                    node.entrance.inner_position = node.content.find_position(
                        inner_point)
                    nodes_found.append(id(node))

        for node_id in nodes_found:
            del untested_nodes[node_id]

    assert not untested_nodes, "cannot find entrances"


def merge_path(outer_path, inner_path, position):
    """
    merge inner path inside outer path at given position.
    Note that since positions contains array indices you
    need to merge starting from last path.
    """
    paths = outer_path.elementary_paths
    outer_point = position.outer_position.point
    inner_point = position.inner_position.point
    if __debug__:
        if is_module_debugged(__name__):
            print("merging at", position.outer_position.index)
            tycat(outer_path, inner_path,
                  position.outer_position.elementary_path)
    arrival_path = paths[position.outer_position.index]
    assert arrival_path.contains(outer_point), "no merging here"

    sub_path = []
    before, after = arrival_path.split_around(outer_point)
    if before is not None:
        sub_path.append(before)

    if not outer_point.is_almost(inner_point):
        sub_path.append(Segment([outer_point, inner_point]))

    sub_path.append(VerticalPath(-1))
    sub_path.extend(inner_path.elementary_paths)
    sub_path.append(VerticalPath(1))

    if not outer_point.is_almost(inner_point):
        sub_path.append(Segment([inner_point, outer_point]))

    if after is not None:
        sub_path.append(after)

    paths[position.outer_position.index:position.outer_position.index] = \
        sub_path
    outer_path.set_elementary_paths(paths)

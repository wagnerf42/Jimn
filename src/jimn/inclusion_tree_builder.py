from jimn.inclusion_tree import inclusion_tree
from jimn.event import event
from jimn.debug import is_module_debugged
from collections import defaultdict


"""
this class builds a tree of polygons included in one another.
it works through a sweeping line algorithms.
also identifies each as a hole or a polygon
"""


class inclusion_tree_builder:
    def __init__(self, polygons):
        self.polygons = polygons
        # prepare for sweeping line algorithm
        self.segments = self.create_segments()
        self.events = self.create_events()
        self.current_segments = defaultdict(list)
        self.seen_polygons = {}
        self.tree = inclusion_tree()
        self.fathers = {}
        # build the inclusion tree
        self.build()

    # get all non-vertical segments in polygons
    def create_segments(self):
        segments = []
        for height, polygons in self.polygons.items():
            for p in polygons:
                segments.extend(p.non_vertical_segments(height))
        return segments

    # create an event for each point belonging to a segment.
    # an event stores two kinds of data :
    #   - segments starting from given point
    #   - segments ending at given point
    def create_events(self):
        events = {}
        for s in self.segments:
            # since segments are oriented in lexicographical order at creation,
            # the indexes of endpoints indicate starting and ending points
            for segment_type, p in enumerate(s.get_endpoints()):
                if p not in events:
                    events[p] = event(p)
                events[p].add_segment(segment_type, s)
        # we sort the events in lexicographical order
        # to prepare for sweeping algorithm
        return sorted(events.values())

    # sweeping algorithm
    def build(self):
        for e in self.events:
            self.handle_event(e)

    def handle_event(self, e):
        # update live segments
        starting_segments, ending_segments = [
            e.get_segments(segment_type) for segment_type in [0, 1]
        ]
        self.update_live_segments(starting_segments, ending_segments)

        # loop through all new segments
        # seeing if we encounter a new polygon never seen before.
        #
        # we sort new segments to ensure all potential inclusions are tested :
        # we have to add the potentially containing polygon first in tree
        for s in sorted(starting_segments,
                        key=lambda seg: (seg.angle(), seg.get_height()),
                        reverse=True):
            polygon_id = s.get_polygon_id()
            if polygon_id not in self.seen_polygons:
                # this guy is new, categorize it

                # add it in tree
                self.add_polygon_in_tree(s)

                # mark it as seen
                self.seen_polygons[polygon_id] = True
                if __debug__:
                    if is_module_debugged(__name__):
                        print("added polygon", s.get_polygon().label,
                              "( h =", s.get_height(), ")")
                        self.tree.tycat()

    def update_live_segments(self, starting_segments, ending_segments):
        for s in ending_segments:
            self.remove_segment(s)
        for s in starting_segments:
            self.add_segment(s)

    def add_segment(self, s):
        # add segment to active segments
        polygon_id = s.get_polygon_id()
        self.current_segments[polygon_id].append(s)

    def remove_segment(self, s):
        # remove segment from active segments.
        polygon_id = s.get_polygon_id()
        self.current_segments[polygon_id].remove(s)
        if not self.current_segments[polygon_id]:
            # the polygon no longer has active segments.
            # delete the entry from active segments.
            del self.current_segments[polygon_id]
            # mark polygon as dead in tree
            father = self.fathers[polygon_id]
            father.kill_child(polygon_id)

    def add_polygon_in_tree(self, new_segment):
        root = self.tree
        # new polygon
        # we first try inserting it at current level ; it might be a hole
        # if not we try going below
        # see report for more help on inclusion trees
        for c in sorted(
            root.get_alive_children(),
            key=lambda c: c.get_height(), reverse=True
        ):
            if self.add_polygon_rec(c, new_segment):
                break
        else:
            self.add_child_in_tree(root, new_segment)

    # see comments in add_polygon_in_tree
    def add_polygon_rec(self, node, new_segment):
        if self.is_included(new_segment, node.get_polygon()):
            for c in sorted(
                node.get_alive_children(),
                key=lambda c: c.get_height(), reverse=True
            ):
                if self.add_polygon_rec(c, new_segment):
                    return True
            if node.is_a_polygon() or new_segment.get_height() == node.get_height():
                self.add_child_in_tree(node, new_segment)
                return True
        return False

    def add_child_in_tree(self, node, new_segment):
        node.add_child(new_segment)
        self.fathers[new_segment.get_polygon_id()] = node

    def is_included(self, new_segment, polygon):
        if id(polygon) not in self.current_segments:
            # this polygon is dead
            # we cannot be included here because new_segment is alive
            return False
        else:
            segments = self.current_segments[id(polygon)]
            if segments[0].get_height() < new_segment.get_height():
                return False
            # do if we are inside, we simply count number of segments
            # traversed when going in one direction (here up)
            # s1 >= s2 means s1 above and higher than s2 (not strictly)
            above_segments = [s for s in segments if s >= new_segment]
            return len(above_segments) % 2 == 1

def build_inclusion_tree(polygons):
    builder = inclusion_tree_builder(polygons)
    return builder.tree

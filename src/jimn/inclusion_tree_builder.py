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
        # small caches to speed up search in tree
        self.last_inserted_node = None
        self.last_inserted_nodes_in_level = {}

        self.build()

    def create_segments(self):
        segments = []
        for height, polygons in self.polygons.items():
            for p in polygons:
                segments.extend(p.non_vertical_segments(height))
        return segments

    def create_events(self):
        events = {}
        for s in self.segments:
            for segment_type, p in enumerate(s.get_endpoints()):
                if p not in events:
                    events[p] = event(p)
                events[p].add_segment(segment_type, s)
        return sorted(events.values())

    def build(self):
        for e in self.events:
            self.handle_event(e)

    def handle_event(self, e):
        starting_segments, ending_segments = [
            e.get_segments(segment_type) for segment_type in [0, 1]
        ]
        self.update_live_segments(starting_segments, ending_segments)

        # loop through all new segments
        # seeing if we encounter a new polygon never seen before
        for s in sorted(starting_segments,
                        key=lambda seg: (seg.angle(), seg.get_height()),
                        reverse=True):
            polygon_id = s.get_polygon_id()
            if polygon_id not in self.seen_polygons:
                #this guy is new, categorize it

                # add it in tree
                self.add_polygon_in_tree(s)

                # mark it as seen
                self.seen_polygons[polygon_id] = True
                if __debug__:
                    if is_module_debugged(__name__):
                        print("adding polygon {} (h={})".format(str(s.get_polygon().label), str(s.get_height())))
                        self.tree.tycat()

    def update_live_segments(self, starting_segments, ending_segments):
        for s in ending_segments:
            self.remove_segment(s)
        for s in starting_segments:
            self.add_segment(s)

    def add_segment(self, s):
        polygon_id = s.get_polygon_id()
        self.current_segments[polygon_id].append(s)

    def remove_segment(self, s):
        polygon_id = s.get_polygon_id()
        self.current_segments[polygon_id].remove(s)
        if self.current_segments[polygon_id] == []:
            del self.current_segments[polygon_id]
            father = self.fathers[polygon_id]
            father.kill_child(polygon_id)

    def add_polygon_in_tree(self, new_segment):
        # we try to insert without searching the whole tree
        if not self.add_polygon_from_cache(new_segment):
            # we failed, search the whole tree
            self.add_polygon_from_root(new_segment)

    def add_polygon_from_root(self, new_segment):
        root = self.tree
        for c in sorted(root.get_alive_children(), key=lambda c: c.get_height(), reverse=True):
            if self.add_polygon_rec(c, new_segment):
                return True
        self.add_child_cached(root, new_segment)
        return True

    def add_polygon_rec(self, node, new_segment):
        if self.is_included(new_segment, node.get_polygon()):
            # TODO: explain why sorted
            for c in sorted(node.get_alive_children(), key=lambda c: c.get_height(), reverse=True):
                if self.add_polygon_rec(c, new_segment):
                    return True
            if node.is_a_polygon() or new_segment.get_height() == node.get_height():
                self.add_child_cached(node, new_segment)
                return True
        return False

    def add_polygon_from_cache(self, new_segment):
        height = new_segment.get_height()
        if self.last_inserted_node is not None and self.fast_insert(self.last_inserted_node, new_segment):
            return True
        if height in self.last_inserted_nodes_in_level and self.fast_insert(self.last_inserted_nodes_in_level[height], new_segment):
            return True
        return False

    def fast_insert(self, father, new_segment):
        if self.is_included(new_segment, father.get_polygon()):  # TODO: mettre is_included comme methode de tree_builder
            print("we are included")
            print(father.is_a_polygon())
            if father.is_a_polygon() or new_segment.get_height() == father.get_height():
                self.add_child_cached(father, new_segment)
                return True
        return False

    def cache_new_tree_node(self, node):
        self.last_inserted_node = node
        self.last_inserted_nodes_in_level[node.get_height()] = node

    def add_child_cached(self, father, new_segment):
        new_node = father.add_child(new_segment)
        self.cache_new_tree_node(new_node)
        self.fathers[new_segment.get_polygon_id()] = father

    def is_included(self, new_segment, polygon):
        print("is segment ", new_segment, " included in poly ", polygon)
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

    def ascend_polygons(self):
        root = self.tree
        for c in root.get_children():
            self.ascend_polygon_rec(c, root, None)

        return self.tree

    def ascend_polygon_rec(self, node, father, grandfather):
        if not node.is_a_polygon():
            moved_children = node.get_children()
            grandfather.get_dead_children().extend(moved_children)
            node.remove_children()
            for c in moved_children:
                self.ascend_polygon_rec(c, grandfather, None)
        else:
            for c in node.get_children():
                self.ascend_polygon_rec(c, node, father)

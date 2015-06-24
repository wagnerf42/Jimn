from jimn.inclusion_tree import inclusion_tree
from jimn.event import event
from jimn.debug import is_module_debugged


class inclusion_tree_builder:
    def __init__(self, polygons):
        self.polygons = polygons
        self.segments = create_segments(self.polygons)
        self.events = create_events(self.segments)
        self.current_segments = {}
        self.seen_polygons = {}
        self.tree = inclusion_tree()
        # small caches to speed up search in tree
        self.last_inserted_node = None
        self.last_inserted_nodes_in_level = {}

        self.build()

    def build(self):
        for e in self.events:
            self.handle_event(e)

    def update_live_segments(self, starting_segments, ending_segments):
        for s in ending_segments:
            remove_segment(s, self.current_segments)
        for s in starting_segments:
            add_segment(s, self.current_segments)

    def cache_new_tree_node(self, node):
        self.last_inserted_node = node
        self.last_inserted_nodes_in_level[node.get_height()] = node

    def add_child_cached(self, father, new_polygon, height):
        new_node = father.add_child(new_polygon, height)
        self.cache_new_tree_node(new_node)

    def add_polygon_in_tree(self, new_polygon, new_segment):
        # we try to insert without searching the whole tree
        if not self.add_polygon_from_cache(new_polygon, new_segment):
            # we failed, search the whole tree
            self.add_polygon_from_root(new_polygon, new_segment, self.current_segments)

    def add_polygon_from_cache(self, new_polygon, new_segment):
        height = new_segment.get_height()
        if self.last_inserted_node is not None and self.fast_insert(self.last_inserted_node, new_polygon, new_segment):
            return True
        if height in self.last_inserted_nodes_in_level and self.fast_insert(self.last_inserted_nodes_in_level[height], new_polygon, new_segment):
            return True
        return False

    def fast_insert(self, father, new_polygon, seg):
        if is_included(seg, father.get_polygon(), self.current_segments):  # TODO: mettre is_included comme methode de tree_builder
            if father.is_a_polygon() or seg.get_height() == father.get_height():
                self.add_child_cached(father, new_polygon, seg.get_height())
                return True
        return False

    def add_polygon_from_root(self, new_polygon, seg, current_segments):
        if self.tree.get_polygon() is None:
            self.tree.__init__(new_polygon, seg.get_height())
            self.cache_new_tree_node(self.tree)
        else:
            self.add_polygon_rec(self.tree, new_polygon, seg, current_segments)

    def add_polygon_rec(self, node, new_polygon, seg, current_segments):
        if is_included(seg, node.get_polygon(), current_segments):  # TODO: mettre is_included comme methode de tree_builder
            # TODO: explain why sorted
            for c in sorted(node.get_children(), key=lambda c: c.get_height(), reverse=True):
                if self.add_polygon_rec(c, new_polygon, seg, current_segments):
                    return True
            if node.is_a_polygon() or seg.get_height() == node.get_height():
                self.add_child_cached(node, new_polygon, seg.get_height())
                return True
        return False

    def handle_event(self, e):
            starting_segments, ending_segments = [e.get_segments(segment_type) for segment_type in [0, 1]]
            self.update_live_segments(starting_segments, ending_segments)

            # loop through all new segments seeing if we encounter a new polygon never seen before
            for s in sorted(starting_segments, key=lambda seg: (seg.angle(), seg.get_height()), reverse=True):
                polygon_id = s.get_polygon_id()
                if polygon_id not in self.seen_polygons:

                    # add it in tree
                    new_polygon = get_polygon(s, self.polygons)
                    self.add_polygon_in_tree(new_polygon, s)

                    # mark it as seen
                    self.seen_polygons[polygon_id] = True
                    if __debug__:
                        if is_module_debugged(__name__):
                            print("adding polygon {} (h={})".format(str(new_polygon.label), str(s.get_height())))
                            self.tree.tycat()

    def ascend_polygons(self):
        super_tree = inclusion_tree()
        super_tree.children = [self.tree]
        ascend_polygon_rec(self.tree, super_tree, None)

        return super_tree


def is_included(new_segment, polygon, current_segments):
    if id(polygon) not in current_segments:
        return False
    else:
        segments = current_segments[id(polygon)]
        if segments[0].get_height() < new_segment.get_height():
            return False
        # s1 >= s2 means s1 above and higher than s2 (not strictly)
        above_segments = [s for s in segments if s >= new_segment]
        return len(above_segments) % 2 == 1


def create_segments(all_polygons):
    all_segments = []
    for height, polygons in all_polygons.items():
        for p in polygons:
            all_segments.extend(p.non_vertical_segments(height))
    return all_segments


def create_events(segments):
    events = {}
    for s in segments:
        for segment_type, p in enumerate(s.get_endpoints()):
            if p not in events:
                events[p] = event(p)
            events[p].add_segment(segment_type, s)
    return sorted(events.values())


def add_segment(s, current_segments):
    polygon_id = s.get_polygon_id()
    if polygon_id not in current_segments:
        current_segments[polygon_id] = [s]
    else:
        current_segments[polygon_id].append(s)


def remove_segment(s, current_segments):
    polygon_id = s.get_polygon_id()
    current_segments[polygon_id].remove(s)
    if current_segments[polygon_id] == []:
        del current_segments[polygon_id]


def get_polygon(segment, polygons):
    polygon_id = segment.get_polygon_id()
    height = segment.get_height()
    same_level_polygons = polygons[height]
    polygon = next(p for p in same_level_polygons if id(p) == polygon_id)
    return polygon


def ascend_polygon_rec(node, father, grandfather):
    if not node.is_a_polygon():
        grandfather.get_children().extend(node.get_children())
        node.remove_children()
    else:
        for c in node.get_children():
            ascend_polygon_rec(c, node, father)

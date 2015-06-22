from jimn.event import event


class inclusion_tree_builder:
    def __init__(self, polygons):
        from jimn.inclusion_tree import inclusion_tree

        self.polygons = polygons
        self.tree = inclusion_tree()
        self.current_segments = {}
        self.seen_polygons = {}

        self.segments = create_segments(self.polygons)
        self.events = create_events(self.segments)

        self.build()

    def build(self):
        for e in self.events:
            self.handle_event(e)

    def handle_event(self, e):
            starting_segments, ending_segments = [e.get_segments(segment_type) for segment_type in [0, 1]]
            for s in ending_segments:
                remove_segment(s, self.current_segments)
            for s in starting_segments:
                add_segment(s, self.current_segments)
            for s in sorted(starting_segments, key=lambda seg: (seg.angle(), seg.get_height()), reverse=True):
                polygon_id = s.get_polygon_id()
                if polygon_id not in self.seen_polygons:
                    new_polygon = get_polygon(s, self.polygons)
                    print("adding polygon {} (h={})".format(str(new_polygon.label), str(s.get_height())))
                    self.tree.add_polygon(new_polygon, s, self.current_segments)
                    self.seen_polygons[polygon_id] = True
                    self.tree.tycat()


def is_included(seg, polygon, current_segments):
    if id(polygon) not in current_segments:
        return False
    else:
        segments = current_segments[id(polygon)]
        # s1 >= s2 means s1 above and higher than s2 (not strictly)
        above_segments = [s for s in segments if s >= seg]
        return len(above_segments) % 2 == 1


def create_segments(all_polygons):
    all_segments = []
    for height, polygons in all_polygons.items():
        for p in polygons:
            all_segments += p.non_vertical_segments(height)
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


def get_polygon(segment, polygons):
    polygon_id = segment.get_polygon_id()
    height = segment.get_height()
    same_level_polygons = polygons[height]
    polygon = next(p for p in same_level_polygons if id(p) == polygon_id)
    return polygon

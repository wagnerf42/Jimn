# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.event import event
import os
import getpass


dot_count = 0


class inclusion_tree:
    """stores a set of polygons included one inside another"""
    def __init__(self, contained_polygon=None, height=None):
        self.polygon = contained_polygon
        self.height = height
        self.children = []

    def add_polygon(self, new_polygon, height, curr_point, curr_segs):
        print("id1: {}".format(id(new_polygon)))
        if self.polygon is None:
            self.polygon = new_polygon
            self.height = height
        else:
            self.add_polygon_rec(new_polygon, height, curr_point, curr_segs)

    def add_polygon_rec(self, new_polygon, height, curr_point, curr_segs):
        if not is_included(curr_point, self.polygon, curr_segs):
            return False
        else:
            # TODO: check height order (if same as natural order or not)
            for c in sorted(self.children, key=lambda c: c.height):
                if c.add_polygon_rec(new_polygon, height, curr_point, curr_segs):
                    return True
            self.add_child(new_polygon, height)
            return True

    def add_child(self, new_polygon, height):
        leaf = inclusion_tree(new_polygon, height)
        self.children.append(leaf)

    def tycat(self):
        global dot_count
        user = getpass.getuser()
        directory = "/tmp/{}".format(user)
        if not os.path.exists(directory):
            os.makedirs(directory)
        dot_file = "{}/{}.dot".format(directory, dot_count)
        svg_file = "{}/{}.svg".format(directory, dot_count)
        dot_count = dot_count + 1
        dot_fd = open(dot_file, 'w')
        dot_fd.write("digraph g {\n")
        self.save_dot(dot_fd)
        dot_fd.write("}")
        dot_fd.close()
        os.system("dot -Tsvg {} -o {}".format(dot_file, svg_file))
        os.system("tycat {}".format(svg_file))

    def save_dot(self, fd):
        fd.write("n{} [label=\"{}, h={}\"];\n".format(id(self), str(self.polygon.label), str(self.height)))
        for child in self.children:
            if child is not None:
                fd.write("n{} -> n{};\n".format(id(self), id(child)))
                child.save_dot(fd)


# couper le fichier en 2: juste l'arbre ; une autre classe que le construit inclusion_tree_builder
def create_tree(polygons):
    # TODO: add polygonsegments hash function
    #       (should be symmetric)
    unique_segments = create_unique_segments(polygons) # pas besoin unicite ; appeller direct method de polygon
    oriented_segments = [s.sort_endpoints() for s in unique_segments] # ne pas faire ici
    events = create_events(oriented_segments)
    sorted_events = sorted(events)

    tree = inclusion_tree() # mettre les structs dans self
    curr_segs = {}
    seen_polygons = {}
    # methode build()
    for e in sorted_events: # appeller une fonction handle event
        curr_point = e.get_event_point()
        beg_segs, end_segs = tuple(e.get_segments(segment_type) for segment_type in [0, 1])
        for s in end_segs:
            remove_segment(s, curr_segs)
        for s in beg_segs:
            add_segment(s, curr_segs)
        for s in beg_segs:
            polygon_id = s.get_polygon_id()
            if polygon_id not in seen_polygons:
                new_polygon = find_polygon(s, polygons)
                print("adding polygon {} (h={})".format(str(new_polygon.label), str(s.get_height())))
                print("id : {}".format(id(new_polygon)))
                print("id0: {}".format(polygon_id))
                tree.add_polygon(new_polygon, s.get_height(), curr_point, curr_segs)
                seen_polygons[polygon_id] = True
                tree.tycat()
    return tree


def is_included(curr_point, polygon, curr_segs):
    if id(polygon) in curr_segs:
        print("id2: {}".format(id(polygon)))
        segments = curr_segs[id(polygon)]
        print(curr_point)
        print([str(s) for s in segments])
        below_segments = [s for s in segments if s.is_below(curr_point)]
        print([str(s) for s in below_segments])
        return len(below_segments) % 2 == 1
    else:
        return False


def create_events(oriented_segments):
    events = {}
    for s in oriented_segments:
        for segment_type, p in enumerate(s.get_endpoints()):
            if p not in events:
                events[p] = event(p)
            events[p].add_segment(segment_type, s)
    return events.values()


def create_unique_segments(polygons):
    unique_segments = {}
    for height, same_level_polygons in polygons.items():
        for polygon in same_level_polygons:
            segments = polygon.polygonsegments(height)
            for s in segments:
                if s not in unique_segments:
                    unique_segments[s] = True
    return unique_segments.keys()


def add_segment(s, curr_segs):
    polygon_id = s.get_polygon_id()
    if polygon_id not in curr_segs:
        curr_segs[polygon_id] = [s]
    else:
        curr_segs[polygon_id].append(s)


def remove_segment(s, curr_segs):
    polygon_id = s.get_polygon_id()
    curr_segs[polygon_id].remove(s)


def find_polygon(segment, polygons):
    polygon_id = segment.get_polygon_id()
    height = segment.get_height()
    same_level_polygons = polygons[height]
    polygon = next(p for p in same_level_polygons if id(p) == polygon_id)
    return polygon

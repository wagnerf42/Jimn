from jimn.coordinates_hash import coordinates_hash
from jimn.segment import segment
from jimn.polygon import NoVertex
from jimn.displayable import tycat
from jimn.iterators import all_pairs
from collections import defaultdict
from math import floor, ceil


class holed_polygon:
    def __init__(self, polygon, height=None, holes=[]):
        self.polygon = polygon
        self.holes = holes
        self.height = height

    def get_bounding_box(self):
        return self.polygon.get_bounding_box()

    def save_svg_content(self, display, color):
        self.polygon.save_svg_content(display, color)
        for hole in self.holes:
            hole.save_svg_content(display, color)

    def normalize(self):
        self.polygon.orient(clockwise=True)
        self.polygon.normalize_starting_point()
        for h in self.holes:
            h.orient(clockwise=False)
            h.normalize_starting_point()
        self.holes = sorted(self.holes, key=lambda h: h.get_points()[0])

    def is_translated(self, p2):
        if not self.polygon.is_translated(p2.polygon):
            return False
        if len(self.holes) != len(p2.holes):
            return False
        for h1, h2 in zip(self.holes, p2.holes):
            if not h1.is_translated(h2):
                return False
        return True

    def round_points(self, rounder):
        self.polygon.round_points(rounder)
        for h in self.holes:
            h.round_points(rounder)

    def milling_heights(self, milling_diameter):
        box = self.get_bounding_box()
        ymin, ymax = box.limits(1)
        start = floor(ymin / milling_diameter)
        end = ceil(ymax / milling_diameter)
        for i in range(start, end+1):
            yield i * milling_diameter

    def build_graph(self, milling_diameter):
        vertices = []
        rounder = coordinates_hash(2)
        for y in self.milling_heights(milling_diameter):
            rounder.hash_coordinate(1, y)

        self.round_points(rounder)

        try:
            vertices.extend(self.polygon.create_vertices(milling_diameter))
        except NoVertex:
            print("TODO: small polygon between two slices")
            raise
        for h in self.holes:
            vertices.extend(h.create_vertices(milling_diameter))

        create_slice_edges(vertices)

        return vertices

    def tycat(self, border):
        tycat(border, self.polygon, *(self.holes))


def create_slice_edges(vertices):
    # we group vertices per height
    vertices_per_height = defaultdict(list)
    for v in vertices:
        vertices_per_height[v.get_y()].append(v)

    # add horizontal edges, so loop on each slice
    for y, same_height_vertices in vertices_per_height.items():
        assert len(same_height_vertices) % 2 == 0, "we cannot have an odd number of aligned vertices"
        # we sort same height vertices
        vertices = sorted(same_height_vertices, key=lambda v: v.get_x())
        # we group same height vertices in linked pairs
        for v1, v2 in all_pairs(vertices):
            l = segment([v1, v2])
            v1.add_link(l)
            v2.add_link(l.reverse())

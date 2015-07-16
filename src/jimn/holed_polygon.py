from jimn.segment import segment
from jimn.polygon import NoVertex
from jimn.displayable import tycat
from collections import defaultdict


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

    def build_graph(self, milling_diameter):
        vertices = []

        try:
            vertices.extend(self.polygon.create_vertices(milling_diameter))
        except NoVertex:
            print("no vertex in polygon")
            raise

        try:
            for h in self.holes:
                vertices.extend(h.create_vertices(milling_diameter))
        except NoVertex:
            print("no vertex in hole")
            raise

        create_slice_links(vertices)

        return vertices

    def tycat(self, border):
        tycat(border, self.polygon, *(self.holes))


def create_slice_links(vertices):
    # we group vertices per height
    vertices_per_height = defaultdict(list)
    for v in vertices:
        # TODO: rounding coordinates beforehand
        vertices_per_height[round(v.get_y(), 7)].append(v)

    # we sort same height vertices
    for y, same_height_vertices in vertices_per_height.items():
        assert len(same_height_vertices) % 2 == 0
        vertices_per_height[y] = sorted(same_height_vertices, key=lambda v: v.get_x())

    # we group same height vertices in linked pairs
    for y, same_height_vertices in vertices_per_height.items():
        even_vertices = same_height_vertices[0:][::2]
        odd_vertices = same_height_vertices[1:][::2]
        for v1, v2 in zip(even_vertices, odd_vertices):
            l = segment([v1, v2])
            v1.add_link(l)
            v2.add_link(l)

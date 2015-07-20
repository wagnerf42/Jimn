from jimn.coordinates_hash import coordinates_hash
from jimn.displayable import tycat
from jimn.graph import graph


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
        self.polygon.orient(clockwise=False)
        self.polygon.normalize_starting_point()
        for h in self.holes:
            h.orient(clockwise=True)
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
        return self.polygon.milling_heights(milling_diameter)

    def build_graph(self, milling_diameter):
        # round all points on intersecting lines
        rounder = coordinates_hash(2)
        for y in self.milling_heights(milling_diameter):
            rounder.hash_coordinate(1, y)

        self.round_points(rounder)

        # fill all vertices
        g = graph()
        self.polygon.create_vertices(milling_diameter, g)
        for h in self.holes:
            h.create_vertices(milling_diameter, g)

        # finish by adding horizontal internal edges
        g.create_internal_edges(milling_diameter)

        return g

    def tycat(self, border):
        tycat(border, self.polygon, *(self.holes))

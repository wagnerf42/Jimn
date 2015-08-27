from jimn.displayable import tycat
from jimn.graph import graph
from jimn.graph.even_degrees import make_degrees_even
from jimn.graph.internal_edges import create_internal_edges
from jimn.utils.coordinates_hash import coordinates_hash


class holed_polygon:
    def __init__(self, polygon, height=None, holes=None):
        self.polygon = polygon
        if holes is None:
            self.holes = []
        else:
            self.holes = holes
        self.height = height

    def get_polygons(self):
        """
        returns a list of all polygons we contain
        (both outer edge and holes)
        """
        polygons = list(self.holes)
        polygons.append(self.polygon)
        return polygons

    def get_bounding_box(self):
        return self.polygon.get_bounding_box()

    def save_svg_content(self, display, color):
        self.polygon.save_svg_content(display, color)
        for hole in self.holes:
            hole.save_svg_content(display, color)

    def normalize(self):
        """
        prepares for hashing by reordering points and
        re-orienting
        """
        self.polygon.orient(clockwise=False)
        self.polygon.normalize_starting_point()
        for h in self.holes:
            h.orient(clockwise=True)
            h.normalize_starting_point()
        self.holes = sorted(self.holes, key=lambda h: h.get_points()[0])

    def is_translated(self, p2):
        """
        are we a translation of p2 ?
        """
        if not self.polygon.is_translated(p2.polygon):
            return False
        if len(self.holes) != len(p2.holes):
            return False
        for h1, h2 in zip(self.holes, p2.holes):
            if not h1.is_translated(h2):
                return False
        return True

    def round_points(self, rounder):
        """
        round all points through given rounder
        """
        self.polygon.round_points(rounder)
        for h in self.holes:
            h.round_points(rounder)

    def milling_heights(self, milling_diameter):
        """
        returns iterator on all y coordinates used in milling
        """
        return self.polygon.milling_heights(milling_diameter)

    def build_graph(self, milling_diameter):
        """
        returns graph which will be used to compute milling path
        """
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
        create_internal_edges(g, milling_diameter)

        #prepare for eulerian path
        make_degrees_even(g)

        return g

    def tycat(self, border):
        tycat(border, self.polygon, *(self.holes))

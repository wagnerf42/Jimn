"""
(multi) edges in graph.
"""
from math import floor
from jimn.elementary_path import Elementary_Path


class Edge:
    """
    contains two vertices, the real underlying path, multiplicity.
    """
    def __init__(self, start_vertex, end_vertex, real_path):
        self.vertices = [start_vertex, end_vertex]
        self.path = real_path
        self.multiplicity = 1
        self.weight = self.path.length()
        assert isinstance(self.path, Elementary_Path)

    def change_multiplicity(self, change):
        """
        update multiplicity.
        """
        assert change == 1 or change == -1
        self.multiplicity += change
        if self.multiplicity == 3:
            self.multiplicity = 1
        self.weight *= -1
        return self.multiplicity

    def get_bounding_box(self):
        """
        bounding box of underlying path.
        """
        return self.path.get_bounding_box()

    def save_svg_content(self, display, color):
        """
        svg for tycat.
        """
        self.path.save_svg_content(display, color)

    def reverse(self):
        """
        generate reversed self.
        """
        return Edge(self.vertices[1], self.vertices[0], self.path.reverse())

    def get_destination(self):
        """
        return destination vertex.
        """
        return self.vertices[1]

    def is_same(self, other):
        """
        are we connecting the same vertices as the other edge ?
        """
        if self.vertices == other.vertices:
            return True
        if self.vertices == list(reversed(other.vertices)):
            return True
        return False

    def is_above_y(self, y_limit):
        """
        are we above or below horizontal segment at y_limit ?
        prerequisite: one of our enpoints is at y_limit.
        """
        non_limit_y = None
        for point in self.path.endpoints:
            point_y = point.get_y()
            if point_y != y_limit:
                non_limit_y = point_y
        assert non_limit_y is not None, "horizontal path"
        return non_limit_y < y_limit

    def is_almost_horizontal(self):
        """
        is underlying path almost horizontal ?
        """
        return self.path.is_almost_horizontal()

    def slice_number(self, milling_diameter):
        """
        space is cut by milling diameter into slices.
        each edge can only be contained in one slice.
        return index of slice containing us.
        """
        points = [v.get_object() for v in self.vertices]
        indexes = [p.get_y()/milling_diameter for p in points]
        index = floor(sum(indexes)/2)
        return index

    def remove(self):
        """
        remove self from graph.
        needs to be non-frontier.
        """
        for i, vertex in enumerate(self.vertices):
            vertex.remove_edge_to(self.vertices[1-i])

    def __lt__(self, other):
        return self.weight < other.weight

    def __hash__(self):
        # TODO: change
        return hash(id(self.vertices[1]))

    def __str__(self):
        return str(id(self.vertices[0])) + " -> " + str(id(self.vertices[1])) \
            + " " + str(self.multiplicity)

    def __eq__(self, other):
        return self.vertices[0] == other.vertices[0] \
            and self.vertices[1] == other.vertices[1]

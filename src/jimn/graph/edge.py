from jimn.elementary_path import elementary_path


class edge:
    def __init__(self, start_vertex, end_vertex, real_path):
        self.vertices = [start_vertex, end_vertex]
        self.path = real_path
        assert isinstance(self.path, elementary_path)

    def get_bounding_box(self):
        return self.path.get_bounding_box()

    def save_svg_content(self, display, color):
        self.path.save_svg_content(display, color)

    def reverse(self):
        return edge(self.vertices[1], self.vertices[0], self.path.reverse())

    def get_weight(self):
        multiplicity = self.vertices[0].get_edge_multiplicity(self)
        if multiplicity % 2:
            return self.path.length()
        else:
            return -self.path.length()

    def get_path(self):
        return self.path

    def get_endpoint(self, index):
        return self.vertices[index]

    def get_endpoints(self):
        return self.vertices

    def get_destination(self):
        return self.vertices[1]

    def is_same(self, other):
        if self.vertices == other.vertices:
            return True
        if self.vertices == list(reversed(other.vertices)):
            return True
        return False

    def is_above_y(self, y_limit):
        """
        are we above or below horizontal segment at y_limit ?
        prerequisite: one of our enpoints is at y_limit
        """
        non_limit_y = None
        for p in self.path.endpoints:
            y = p.get_y()
            if y != y_limit:
                non_limit_y = y
        assert non_limit_y is not None, "horizontal path"
        return non_limit_y < y_limit

    def is_horizontal(self):
        return self.path.is_horizontal()

    def __hash__(self):
        return hash(id(self.vertices[1]))

    def __str__(self):
        return str(id(self.vertices[0])) + " -> " + str(id(self.vertices[1]))

    def __eq__(self, other):
        return self.vertices[0] == other.vertices[0] \
            and self.vertices[1] == other.vertices[1]

from jimn.displayable import tycat


class holed_polygon:
    def __init__(self, polygon, height=None, holes=[]):
        self.polygon = polygon
        self.holes = holes
        self.height = height

    def normalize(self):
        self.polygon.orient(clockwise=True)
        self.polygon.normalize_starting_point()
        for h in self.holes:
            h.orient(clockwise=False)
            h.normalize_starting_point()
        self.holes = sorted(self.holes, key=lambda h: h.get_points[0])

    def is_translated(self, p2):
        if not self.polygon.is_translated(p2.polygon):
            return False
        if len(self.holes) != len(p2.holes):
            return False
        for h1, h2 in zip(self.holes, p2.holes):
            if not h1.is_translated(h2):
                return False
        return True

    def tycat(self, border):
        tycat(border, self.polygon, *(self.holes))

from jimn.displayable import tycat

class holed_polygon:
    def __init__(self, polygon, height=None, holes=[]):
        self.polygon = polygon
        self.holes = holes
        self.height = height

    def tycat(self, border):
        tycat(border, self.polygon, *(self.holes))

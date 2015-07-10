from jimn.displayable import tycat
from jimn.vertex import vertex


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
        elementary_segments = self.polygon.cut(milling_diameter)
        points = [s.get_endpoint(0) for s in elementary_segments]
        for index, point in enumerate(points):
            if point.is_on_slice(milling_diameter):
                points[index] = vertex(point)

        for index, point in enumerate(points):
            if type(point) is vertex:
                start = index
                break

        prec = points[start]
        link = [elementary_segments[start]]
        n = len(points)
        k = (start + 1) % n

        while k != start:
            if type(points[k]) is not vertex:
                link.append(elementary_segments[k])
                k = (k + 1) % n
            else:
                curr = points[k]
                prec.add_link(link)
                curr.add_link(link)

                prec = curr
                link = [elementary_segments[k]]
                k = (k + 1) % n

        curr = points[k]
        prec.add_link(link)
        curr.add_link(link)

        return points

    def tycat(self, border):
        tycat(border, self.polygon, *(self.holes))

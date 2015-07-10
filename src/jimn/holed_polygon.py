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

        elementary_segments = reorder_elementary_segments_to_start_at_vertex(elementary_segments, milling_diameter)

        vertices = []
        intermediate_path = []
        final_vertex = vertex(elementary_segments[0].get_endpoint(0))
        previous_vertex = final_vertex
        for s in elementary_segments:
            p = s.get_endpoint(1)
            intermediate_path.append(s)
            if p.is_on_slice(milling_diameter):
                v = vertex(p)
                v.add_link(list(reversed(intermediate_path)))
                previous_vertex.add_link(intermediate_path)
                intermediate_path = []
                previous_vertex = v
                vertices.append(v)
        vertices[-1].add_link(final_vertex.get_link())

        return vertices

    def tycat(self, border):
        tycat(border, self.polygon, *(self.holes))


def reorder_elementary_segments_to_start_at_vertex(elementary_segments, milling_diameter):
        # find first vertex
        for index, s in enumerate(elementary_segments):
            p = s.get_endpoint(0)
            if p.is_on_slice(milling_diameter):
                start = index
                break
        else:
            raise "no vertex"

        return (elementary_segments[start:] + elementary_segments[:start])

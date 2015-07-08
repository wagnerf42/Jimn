from jimn.bounding_box import bounding_box


class arc:
    def __init__(self, radius, points):
        self.radius = radius
        self.points = points

    def get_bounding_box(self):
        box = bounding_box.empty_box(2)
        for p in self.points:
            box.add_point(p)
        return box

    def save_svg_content(self, display, color):
        x1, y1, x2, y2 = [
            c for p in self.points
            for c in display.convert_coordinates(p.get_coordinates())
        ]
        r = display.stretch * self.radius
        display.write('<path d="M{},{} A{},{} 0 0,1 {},{}" \
                      fill="none" stroke="{}" \
                      opacity="0.5" stroke-width="3"\
                      />'.format(x1, y1, r, r, x2, y2, color))

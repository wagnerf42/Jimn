# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

class point:

    def __init__(self, *arg):
        self.coordinates = arg

    def __str__(self):
        return "({})".format(','.join(map(lambda x: str(x), self.coordinates)))

    def get_coordinates(self):
        return self.coordinates

    def get_bounding_box(self):
        return [self.coordinates, self.coordinates]

    #careful : only works on 2d points
    def save_svg_content(self, display, color):
        svg_coordinates = display.convert_coordinates(self.coordinates)
        display.write("<circle cx=\"{}\" cy=\"{}\" r=\"5\" fill=\"{color_arg}\"/>\n".format(*svg_coordinates, color_arg=color))


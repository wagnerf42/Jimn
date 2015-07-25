import os
import getpass
from jimn.bounding_box import bounding_box
from math import ceil

svg_dimensions = (400, 200)
margin = 20

svg_colors = tuple('red green blue purple orange saddlebrown mediumseagreen\
                   darkolivegreen darkred dimgray mediumpurple midnightblue\
                   olive chartreuse darkorchid hotpink lightskyblue peru\
                   goldenrod mediumslateblue orangered darkmagenta\
                   darkgoldenrod mediumslateblue firebrick palegreen\
                   royalblue tan tomato springgreen pink orchid\
                   saddlebrown moccasin mistyrose cornflowerblue\
                   darkgrey'.split())


class displayed_thing(object):
    def __init__(self, things):
        self.bounding_box = bounding_box.empty_box(2)
        for thing in things:
            if (type(thing) is list) or (type(thing) is tuple):
                for subthing in thing:
                    if subthing is not None:
                        self.bounding_box.update(subthing.get_bounding_box())
            else:
                if thing is not None:
                    self.bounding_box.update(thing.get_bounding_box())
        self._calibrate()

    def _calibrate(self):
        """
        compute how to transform coordinates
        """
        coordinates = self.bounding_box.get_arrays()
        self.min_coordinates, self.max_coordinates = coordinates
        dimensions = [
            a - b for a, b in zip(self.max_coordinates, self.min_coordinates)
        ]
        real_dimensions = [d-2*margin for d in svg_dimensions]
        stretches = [a / b for a, b in zip(real_dimensions, dimensions)]
        self.svg_stretch = min(stretches)
        self.margins = [
            (a-b*self.svg_stretch)/2 for a, b in zip(svg_dimensions, dimensions)
        ]

    def _open_svg(self, filename):
        """
        open new svg file
        """
        self.fd = open(filename, 'w')
        self.fd.write("<svg width=\"{}\"\
                      height=\"{}\">\n".format(*svg_dimensions))
        self.fd.write("<rect width=\"{}\" height=\"{}\"\
                      fill=\"white\"/>\n".format(*svg_dimensions))

    def _close_svg(self):
        """
        close svg file
        """
        self.fd.write("</svg>\n")
        self.fd.close()

    def stretch(self):
        """
        returns stretch required for converting to svg coordinates
        """
        return self.svg_stretch

    def convert_coordinates(self, coordinates):
        """
        convert coordinates to svg coordinates
        """
        relative_coordinates = [
            a - b for a, b in zip(coordinates, self.min_coordinates)
        ]
        return [
            a+b*self.svg_stretch
            for a, b in zip(self.margins, relative_coordinates)
        ]

    def write(self, string):
        """
        write a string to svg file
        """
        self.fd.write(string)

    def get_color(self, index):
        return svg_colors[index % len(svg_colors)]

    def stroke_width(self):
        min_dimension = min(svg_dimensions)
        expected_size = ceil(min_dimension / 100)
        return expected_size

file_count = 0


def tycat(*things):
    """
    graphically displays a list of objects.
    requires :
        - the terminology terminal emulator
        - each object displays implements
            * get_bounding_box
            * save_svg_content
    """
    global file_count

    try:
        display = displayed_thing(things)
    except ZeroDivisionError:
        print("*** svg display failed ***")
        return

    user = getpass.getuser()
    directory = "/tmp/{}".format(user)
    if not os.path.exists(directory):
        os.makedirs(directory)
    filename = "{}/{}.svg".format(directory, str(file_count))

    display._open_svg(filename)

    color_index = 0
    for thing in things:
        color = svg_colors[color_index]
        if (type(thing) is list) or (type(thing) is tuple):
            for subthing in thing:
                if subthing is not None:
                    subthing.save_svg_content(display, color)
        else:
            if thing is not None:
                thing.save_svg_content(display, color)
        color_index = (color_index+1) % len(svg_colors)

    display._close_svg()

    os.system("convert {} {}.jpg".format(filename, filename))
    os.system("tycat {}.jpg".format(filename))
    # os.system("tycat {}".format(filename))
    file_count = file_count + 1


def tycat_set_svg_dimensions(w, h):
    """
    sets dimension (screen size) of svg images displayed
    """
    global svg_dimensions
    svg_dimensions = (w, h)

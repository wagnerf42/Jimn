"""
graphical display system.
save objects sets as svg files and view them in terminology
"""

from math import ceil
import os
import getpass
from jimn.bounding_box import Bounding_Box

FILE_COUNT = 0


class Displayer:
    """
    displayer handles computations for displaying a set of objects
    """
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

    def __init__(self, filename, things):
        self.filename = filename
        self.svg_file = None
        self.bounding_box = Bounding_Box.empty_box(2)
        for thing in things:
            if isinstance(thing, list) or isinstance(thing, tuple):
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
        real_dimensions = [d-2*self.margin for d in self.svg_dimensions]
        adjusted_dimensions = []
        for size in dimensions:
            if size == 0:
                adjusted_dimensions.append(0.001)
            else:
                adjusted_dimensions.append(size)

        stretches = [
            a / b for a, b in zip(real_dimensions, adjusted_dimensions)
        ]
        self.svg_stretch = min(stretches)
        self.margins = [
            (a-b*self.svg_stretch)/2
            for a, b in zip(self.svg_dimensions, adjusted_dimensions)
        ]

    def open_svg(self, filename):
        """
        open new svg file
        """
        self.svg_file = open(filename, 'w')
        self.svg_file.write("<svg width=\"{}\"\
                      height=\"{}\">\n".format(*self.svg_dimensions))
        self.svg_file.write("<rect width=\"{}\" height=\"{}\"\
                      fill=\"white\"/>\n".format(*self.svg_dimensions))

    def close_svg(self):
        """
        close svg file
        """
        self.svg_file.write("</svg>\n")
        self.svg_file.close()

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
        self.svg_file.write(string)

    def stroke_width(self):
        """
        return size of stroke to use in svg
        """
        min_dimension = min(self.svg_dimensions)
        expected_size = ceil(min_dimension / 150)
        return expected_size

    def svg_color(self, index):
        """
        return color corresponding to given index
        """
        return self.svg_colors[index % len(self.svg_colors)]

    def svg_color_after(self, color, shift):
        """
        returns 'shift' colors after given color
        """
        color_index = self.svg_colors.index(color)
        color_index += shift
        return self.svg_colors[color_index % len(self.svg_colors)]


def tycat_start(*things):
    """
    open svg file ; prepare display
    """
    global FILE_COUNT
    print("[", FILE_COUNT, "]")
    dimensions = os.environ.get("JIMN_TYCAT_SIZE")

    if dimensions is not None:
        width, height = [int(s) for s in dimensions.split("x")]
        tycat_set_svg_dimensions(width, height)

    user = getpass.getuser()
    directory = "/tmp/{}".format(user)
    if not os.path.exists(directory):
        os.makedirs(directory)
        filename = "{}/{}.svg".format(directory, str(FILE_COUNT).zfill(5))
    FILE_COUNT += 1

    display = Displayer(filename, things)
    display.open_svg(filename)
    return display


def tycat(*things):
    """
    graphically displays a list of objects.
    requires :
        - the terminology terminal emulator
        - each object displays implements
            * get_bounding_box
            * save_svg_content
    """

    display = tycat_start(things)
    color_index = 0
    for thing in things:
        color = display.svg_colors[color_index]
        if isinstance(thing, list) or isinstance(thing, tuple):
            for subthing in thing:
                if subthing is not None:
                    subthing.save_svg_content(display, color)
        else:
            if thing is not None:
                thing.save_svg_content(display, color)
        color_index = (color_index+1) % len(display.svg_colors)

    tycat_end(display)


def tycat_end(display):
    """
    complete the svg file and close it
    """
    display.close_svg()

    os.system("convert {} {}.jpg".format(display.filename, display.filename))
    os.system("tycat {}.jpg".format(display.filename))
    # os.system("tycat {}".format(filename))


def tycat_set_svg_dimensions(width, height):
    """
    sets dimension (screen size) of svg images displayed
    """
    Displayer.svg_dimensions = (width, height)

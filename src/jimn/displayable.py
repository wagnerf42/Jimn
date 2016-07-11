"""
graphical display system.
save objects sets as svg files and view them in terminology
"""
from tempfile import NamedTemporaryFile
from math import ceil
import os
import getpass
from jimn.bounding_box import BoundingBox


class Displayer:
    """
    displayer handles computations for displaying a set of objects
    """
    svg_dimensions = (800, 600)
    margin = 20
    svg_colors = tuple('red green blue purple orange saddlebrown mediumseagreen\
                       darkolivegreen lightskyblue dimgray mediumpurple midnightblue\
                       olive chartreuse darkorchid hotpink darkred peru\
                       goldenrod mediumslateblue orangered darkmagenta\
                       darkgoldenrod mediumslateblue firebrick palegreen\
                       royalblue tan tomato springgreen pink orchid\
                       saddlebrown moccasin mistyrose cornflowerblue\
                       darkgrey'.split())
    file_count = 0

    def __init__(self, filename, things, bounding_box):
        self.filename = filename
        self.svg_file = None
        if bounding_box is None:
            self.bounding_box = BoundingBox.empty_box(2)
            for thing in things:
                if isinstance(thing, list) or isinstance(thing, tuple):
                    for subthing in thing:
                        if subthing is not None:
                            self.bounding_box.update(
                                subthing.get_bounding_box())
                else:
                    if thing is not None:
                        self.bounding_box.update(thing.get_bounding_box())

        else:
            self.bounding_box = bounding_box

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
        expected_size = ceil(min_dimension / 500)
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

    def __hash__(self):
        """
        WARNING : only hash things useful for coordinates computations.
        """
        return hash(tuple(self.min_coordinates)) ^ hash(self.svg_stretch) ^ \
            hash(tuple(self.margins))

    def __eq__(self, other):
        """
        WARNING : only compare things useful for coordinates computations.
        """
        if self.svg_stretch != other.svg_stretch:
            return False
        if self.min_coordinates != other.min_coordinates:
            return False
        if self.margins != other.margins:
            return False
        return True


def tycat_start(things, bounding_box=None):
    """
    open svg file ; prepare display.
    use bounding box if there is one. else things.
    """
    print("[", Displayer.file_count, "]")
    dimensions = os.environ.get("JIMN_TYCAT_SIZE")

    if dimensions is not None:
        width, height = [int(s) for s in dimensions.split("x")]
        tycat_set_svg_dimensions(width, height)

    user = getpass.getuser()
    directory = "/tmp/{}".format(user)
    if not os.path.exists(directory):
        os.makedirs(directory)

    filename = "{}/{}.svg".format(directory, str(Displayer.file_count).zfill(5))
    Displayer.file_count += 1

    display = Displayer(filename, things, bounding_box)
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


def gnuplot(data):
    """
    display given multi-dimensional array with gnuplot.
    """
    with NamedTemporaryFile(mode="w") as data_file:
        for line in data:
            strings = [str(l) for l in line]
            print(" ".join(strings), file=data_file)

        data_file.flush()
        command = "gnuplot -e \"set terminal pngcairo ;"
        command += " set output \\\"/tmp/test.png\\\" ;"
        command += " plot "
        for index in range(1, len(data[0])):
            command += \
                " \\\"{}\\\" using 1:{} with linespoints, ".format(
                    data_file.name, index+1)
        command += "\""
        os.system(command)
        os.system("tycat /tmp/test.png")

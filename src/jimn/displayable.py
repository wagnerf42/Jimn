"""
graphical display system.
save objects sets as svg files and view them in terminology
"""
from tempfile import NamedTemporaryFile
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
        self.dimensions = [
            a - b for a, b in zip(self.max_coordinates, self.min_coordinates)
        ]

    def open_svg(self, filename):
        """
        open new svg file
        """
        self.svg_file = open(filename, 'w')
        ratios = [a/b for a, b in zip(self.svg_dimensions, self.dimensions)]
        scale = min(ratios)
        stroke = 3/scale
        self.svg_file.write('<svg width="{}" height="{}"'.format(*self.svg_dimensions))
        self.svg_file.write(' viewBox="{} {}'.format(*self.min_coordinates))
        self.svg_file.write(' {} {}"'.format(*self.dimensions))
        self.svg_file.write(' xmlns:xlink="http://www.w3.org/1999/xlink">\n')
        self.svg_file.write('<rect x="{}" y="{}"'.format(*self.min_coordinates))
        self.svg_file.write(' width="{}" height="{}" fill="white"/>\n'.format(*self.dimensions))
        self.svg_file.write('<defs><symbol id="c"><circle r="{}"/></symbol></defs>\n'.format(2*stroke))
        self.svg_file.write('<g stroke-width="{}" opacity="0.7">\n'.format(stroke))

    def close_svg(self):
        """
        close svg file
        """
        self.svg_file.write("</g>\n")
        self.svg_file.write("</svg>\n")
        self.svg_file.close()

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


def tycat(*things, bounding_box=None):
    """
    graphically displays a list of objects.
    requires :
        - the terminology terminal emulator
        - each object displays implements
            * get_bounding_box
            * svg_content
    """

    display = tycat_start(things, bounding_box)
    color_index = 0
    for thing in things:
        color = display.svg_colors[color_index]
        display.svg_file.write('<g fill="{}" stroke="{}">\n'.format(color, color))
        if isinstance(thing, list) or isinstance(thing, tuple):
            for subthing in thing:
                if subthing is not None:
                    if isinstance(subthing, str):
                        display.svg_file.write(subthing)
                    else:
                        display.svg_file.write(subthing.svg_content())
        else:
            if thing is not None:
                display.svg_file.write(thing.svg_content())
        display.svg_file.write('</g>\n')
        color_index = (color_index+1) % len(display.svg_colors)

    tycat_end(display)


def tycat_end(display):
    """
    complete the svg file and close it
    """
    display.close_svg()

    # os.system("convert {} {}.jpg".format(display.filename, display.filename))
    # os.system("tycat {}.jpg".format(display.filename))
    os.system("tycat {}".format(display.filename))


def tycat_set_svg_dimensions(width, height):
    """
    sets dimension (screen size) of svg images displayed
    """
    Displayer.svg_dimensions = (width, height)


def gnuplot(labels, data):
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
        for index, label in enumerate(labels):
            command += \
                " \\\"{}\\\" using 1:{} with linespoints title \\\"{}\\\",".format(
                    data_file.name, index+2, label)
        command += "\""
        os.system(command)
        os.system("tycat /tmp/test.png")

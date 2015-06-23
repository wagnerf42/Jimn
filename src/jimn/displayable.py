# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
import os
import getpass
import sys
from jimn.bounding_box import bounding_box

svg_dimensions = (400, 200)
margin = 20

svg_colors = tuple('red green blue purple orange saddlebrown mediumseagreen darkolivegreen darkred dimgray mediumpurple midnightblue olive chartreuse darkorchid hotpink lightskyblue peru goldenrod mediumslateblue orangered darkmagenta darkgoldenrod mediumslateblue firebrick palegreen royalblue tan tomato springgreen pink orchid saddlebrown moccasin mistyrose  cornflowerblue darkgrey'.split())


class displayed_thing(object):
    def __init__(self, things):
        self.bounding_box = bounding_box.empty_box(2)
        for thing in things:
            if (type(thing) is list) or (type(thing) is tuple):
                for subthing in thing:
                    self.bounding_box.update(subthing.get_bounding_box())
            else:
                self.bounding_box.update(thing.get_bounding_box())
        self.calibrate()

    def calibrate(self):
        self.max_coordinates, self.min_coordinates = self.bounding_box.get_arrays()
        dimensions = [a - b for a, b in zip(self.max_coordinates, self.min_coordinates)]
        real_dimensions = [d-2*margin for d in svg_dimensions]
        stretches = [a / b for a, b in zip(real_dimensions, dimensions)]
        self.stretch = min(stretches)
        self.margins = [(a-b*self.stretch)/2 for a, b in zip(svg_dimensions, dimensions)]

    def open_svg(self, filename):
        self.fd = open(filename, 'w')
        self.fd.write('')
        self.fd.write("<svg width=\"{}\" height=\"{}\">\n".format(*svg_dimensions))
        self.fd.write("<rect width=\"{}\" height=\"{}\" fill=\"white\"/>\n".format(*svg_dimensions))

    def close_svg(self):
        self.fd.write("</svg>\n")
        self.fd.close()

    def stretch(self):
        return self.stretch

    def convert_coordinates(self, coordinates):
        relative_coordinates = [a - b for a, b in zip(coordinates, self.min_coordinates)]
        return [a+b*self.stretch for a, b in zip(self.margins, relative_coordinates)]

    def write(self, string):
        self.fd.write(string)

file_count = 0


def tycat(*things):
    global file_count
    user = getpass.getuser()
    directory = "/tmp/{}".format(user)
    if not os.path.exists(directory):
        os.makedirs(directory)
    filename = "{}/{}.svg".format(directory, str(file_count))

    display = displayed_thing(things)

    display.open_svg(filename)

    color_index = 0
    for thing in things:
        color = svg_colors[color_index]
        if (type(thing) is list) or (type(thing) is tuple):
            for subthing in thing:
                subthing.save_svg_content(display, color)
        else:
            if thing is not None:
                thing.save_svg_content(display, color)
        color_index = (color_index+1) % len(svg_colors)

    display.close_svg()

    os.system("convert {} {}.jpg".format(filename, filename))
    os.system("tycat {}.jpg".format(filename))
    #os.system("tycat {}".format(filename))
    file_count = file_count + 1


def tycat_set_svg_dimensions(w, h):
    global svg_dimensions
    svg_dimensions = (w, h)

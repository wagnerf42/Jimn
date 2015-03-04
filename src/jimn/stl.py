# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.point import point
from jimn.facet import facet, binary_facet
import struct


class stl:
    def __init__(self, file_name):
        self.facets = parse_stl(file_name)

    def project_half_space_intersection(self, h):
        segments = []
        for t in self.facets:
            segments.extend(t.intersect(h))
        return segments


def parse_stl(file_name):
    if binary_stl_header(file_name):
        return parse_binary_stl(file_name)
    else:
        return parse_ascii_stl(file_name)


def binary_stl_header(file_name):
    with open(file_name, "rb") as f:
        zeroes_head = f.read(80)
        if not zeroes_head:
            return False
        s = struct.Struct('80c')
        zeroes = s.unpack(zeroes_head)
        for h in zeroes:
            if h != b'\x00':
                return False
        return True


def parse_binary_stl(file_name):
    with open(file_name, "rb") as f:
        f.read(80)
        packed_size = f.read(4)
        if not packed_size:
            return False
        s = struct.Struct('I')
        size = s.unpack(packed_size)[0]
        facets = []
        for i in range(size):
            facets.append(binary_facet(f))
        return facets


def parse_ascii_stl(file_name):
    fd = open(file_name, "r")
    s = fd.read()
    fd.close()
    l = s.split()

    facets = []
    i = 0
    i = parse_begin_solid(l, i)

    while l[i] == "facet":
        i, f = parse_facet(l, i)
        facets.append(f)

    i = parse_end_solid(l, i)

    return facets


def parse_facet(l, i):
    i = parse_begin_facet(l, i)
    i = parse_begin_loop(l, i)
    i, t = parse_facet_content(l, i)
    i = parse_end_loop(l, i)
    i = parse_end_facet(l, i)

    return i, t


def parse(l, i, string):
    if(l[i] != string):
        print("ERREUR : mot {} non reconnu".format(string))
        raise IOError
    i += 1
    return i


def parse_begin_solid(l, i):
    i = parse(l, i, "solid")
    i += 1
    return i


def parse_end_solid(l, i):
    i = parse(l, i, "endsolid")
    i += 1
    return i


def parse_end_loop(l, i):
    i = parse(l, i, "endloop")
    return i


def parse_begin_facet(l, i):
    i = parse(l, i, "facet")
    i = parse(l, i, "normal")

    i += 3
    return i


def parse_end_facet(l, i):
    i = parse(l, i, "endfacet")
    return i


def parse_begin_loop(l, i):
    i = parse(l, i, "outer")
    i = parse(l, i, "loop")
    return i


def parse_point(l, i):
    i = parse(l, i, "vertex")
    coordinates = []
    for j in range(1, 4):
        coordinates.append(float(l[i]))
        i += 1
    p = point(*coordinates)
    return i, p


def parse_facet_content(l, i):
    f = facet()
    for j in range(3):
        i, p = parse_point(l, i)
        f.add_point(p)
    return i, f

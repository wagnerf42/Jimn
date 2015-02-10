# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.point import *
from jimn.segment import *
from jimn.triangle import *

def parse_stl(fileName):
	f = open(fileName, "r")
	s = f.read()
	l = s.split()

	listtriangle = []
	i = 0
	i = parse_begin_solid(l, i)

	while l[i] == "facet":
		i, t = parse_facet(l, i)
		listtriangle.append(t)

	i = parse_end_solid(l, i)
	f.close()

	return listtriangle

def parse_facet(l, i):
	i = parse_begin_facet(l, i)
	i = parse_begin_loop(l, i)
	i, t = parse_triangle(l, i)
	i = parse_end_loop(l, i)
	i = parse_end_facet(l, i)

	return i, t

def parse(l, i, string):
	if(l[i] != string):
		print("ERREUR : mot {} non reconnu".format(string))
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
	i = parse(l, i, "outerloop")
	return i

def parse_end_loop(l, i):
	i = parse(l, i, "endloop")
	return i

def parse_point(l, i):
    i = parse(l, i, "vertex")
    p = point()
    for j in range(1,4):
        p.coord.append(float(l[i]))
        i += 1
    return i, p

def parse_triangle(l, i):
    t = triangle()
    for j in range(0, 3):
        i, t.sommets[j] = parse_point(l, i)
    return i, t

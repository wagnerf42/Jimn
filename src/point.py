class Point2D:
    def __init__(self, x = 0.0, y = 0.0):
        self.x = x
        self.y = y
    def __str__(self):
        return "({}, {})".format(str(self.x), str(self.y))

class Point3D(Point2D):
    def __init__(self, x = 0.0, y = 0.0, z = 0.0):
        Point2D.__init__(self, x, y)
        self.z = z
    def __str__(self):
        return Point2D.__str__(self)[:-1] + ", {})".format(str(self.z))

class Segment:
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2
    def __str__(self):
        return "[{} ; {}]".format(str(self.p1), str(self.p2))

class Triangle(Segment):
    def __init__(self, p1 = Point3D(), p2 = Point3D(), p3 = Point3D()):
        Segment.__init__(self, p1, p2)
        self.p3 = p3
    def __str__(self):
        return Segment.__str__(self)[:-1] + " ; {}]".format(str(self.p3))

def parseStl(fileName):
	f = open(fileName, "r")
	s = f.read()
	l = s.split()
	print("{} {} {} {}".format(l[0], l[1], l[2], l[3]))
	
	listTriangle = []
	i = 0
	i = parseBeginSolid(l, i)

	while l[i] == "facet":
		i, t = parseFacet(l, i)
		listTriangle.append(t)

	i = parseEndSolid(l, i)
	f.close()

	return listTriangle

def parseFacet(l, i):
		i = parseBeginFacet(l, i)
		i = parseBeginLoop(l, i)
		i, t = parseTriangle(l, i)
		i = parseEndLoop(l, i)
		i = parseEndFacet(l, i)

		return i, t

def parse(l, i, string):
	if(l[i] != string):
		print("ERREUR : mot {} non reconnu").format(string)
	i += 1
	return i

def parseBeginSolid(l, i):
	i = parse(l, i, "solid")
	i += 1
	return i

def parseEndSolid(l, i):
	i = parse(l, i, "endsolid")
	i += 1
	return i
	
def parseEndLoop(l, i):
	i = parse(l, i, "endloop")
	return i
	
def parseBeginFacet(l, i):
	i = parse(l, i, "facet")
	i = parse(l, i, "normal")

	i += 3
	return i

def parseEndFacet(l, i):
	i = parse(l, i, "endfacet")
	return i

def parseBeginLoop(l, i):
	i = parse(l, i, "outerloop")
	return i

def parseEndLoop(l, i):
	i = parse(l, i, "endloop")
	return i
	
def parsePoint(l, i):
	i = parse(l, i, "vertex")
	p = Point3D()
	print(i)
	p.x = float(l[i])
	i += 1
	print(i)
	p.y = float(l[i])
	i += 1
	print(i)
	p.z = float(l[i])
	i += 1
	return i, p

def parseTriangle(l, i):
	t = Triangle()
	i, t.p1 = parsePoint(l, i)
	i, t.p2 = parsePoint(l, i)
	i, t.p3 = parsePoint(l, i)
	return i, t

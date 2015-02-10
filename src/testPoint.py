# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.parse_stl import *

p2 = point();
print(str(p2))
p2 = point(1.0, 2.0)
print(str(p2))
q2 = point(3.0, 4.0)
print(str(q2))
r2 = point(5.0, 6.0)
print(str(r2))

s2 = segment(p2, q2)
print(str(s2))

t2 = triangle(p2, q2, r2)
print(str(t2))

p3 = point();
print(str(p3))
p3 = point(1.0, 2.0, 3.0)
print(str(p3))
q3 = point(4.0, 5.0, 6.0)
print(str(q3))

s3 = segment(p3, q3)
print(str(s3))

s = "ceci est une\nligne de texte"
l = s.split()
for x in l:
	print(x)

mot = "bonjour"
if mot == "bonjour":
	print("ok == str")
if mot != "Bonjour":
	print("ok == str")

data = stl("solid.stl")
print(str(data.triangles[0]))
print(str(data.triangles[1]))

t = triangle()
print(str(t))

p = point(3., 4.)
print(str(p.coord[0]) + " " + str(p.coord[1]))

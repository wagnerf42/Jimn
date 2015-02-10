import point

p2 = point.Point2D();
print(str(p2))
p2 = point.Point2D(1.0, 2.0)
print(str(p2))
q2 = point.Point2D(3.0, 4.0)
print(str(q2))
r2 = point.Point2D(5.0, 6.0)
print(str(r2))

s2 = point.Segment(p2, q2)
print(str(s2))

t2 = point.Triangle(p2, q2, r2)
print(str(t2))

p3 = point.Point3D();
print(str(p3))
p3 = point.Point3D(1.0, 2.0, 3.0)
print(str(p3))
q3 = point.Point3D(4.0, 5.0, 6.0)
print(str(q3))

s3 = point.Segment(p3, q3)
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

listTr = point.parseStl("solid.stl")
print(str(listTr[0]))
print(str(listTr[1]))

t = point.Triangle()
print(str(t))

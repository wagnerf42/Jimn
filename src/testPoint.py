import point

p2 = point.point2d();
print(str(p2))
p2 = point.point2d(1.0, 2.0)
print(str(p2))
q2 = point.point2d(3.0, 4.0)
print(str(q2))
r2 = point.point2d(5.0, 6.0)
print(str(r2))

s2 = point.segment(p2, q2)
print(str(s2))

t2 = point.triangle(p2, q2, r2)
print(str(t2))

p3 = point.point3d();
print(str(p3))
p3 = point.point3d(1.0, 2.0, 3.0)
print(str(p3))
q3 = point.point3d(4.0, 5.0, 6.0)
print(str(q3))

s3 = point.segment(p3, q3)
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

listTr = point.parse_stl("solid.stl")
print(str(listTr[0]))
print(str(listTr[1]))

t = point.triangle()
print(str(t))

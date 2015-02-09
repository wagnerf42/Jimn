import point

p2 = point.Point2D();
print(str(p2))
p2 = point.Point2D(1.0, 2.0)
print(str(p2))
q2 = point.Point2D(3.0, 4.0)
print(str(q2))

s2 = point.Segment(p2, q2)

p3 = point.Point3D();
print(str(p3))
p3 = point.Point3D(1.0, 2.0, 3.0)
print(str(p3))


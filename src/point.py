class Point2D:
    def __init__(self, x = 0, y = 0):
        self.x = x
        self.y = y

class Point3D(Point2D):
    def __init__(self, x = 0, y = 0, z = 0):
        Point2D.__init__(self, x, y)
        self.z = z

class Segment:
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2

class Triangle(Segment):
    def __init__(self, p1, p2, p3):
        Segment.__init__(self, p1, p2)
        self.p3 = p3

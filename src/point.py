class Point2D:
    def __init__(self, x = 0, y = 0):
        self.x = x
        self.y = y

class Point3D(Point2D):
    def __init__(self, x = 0, y = 0, z = 0):
        Point2D.__init__(self, x, y)
        self.z = z

class point2d:
    def __init__(self, x = 0.0, y = 0.0):
        self.x = x
        self.y = y
    def __str__(self):
        return "({}, {})".format(str(self.x), str(self.y))

class point3d(point2d):
    def __init__(self, x = 0.0, y = 0.0, z = 0.0):
        point2d.__init__(self, x, y)
        self.z = z
    def __str__(self):
        return point2d.__str__(self)[:-1] + ", {})".format(str(self.z))

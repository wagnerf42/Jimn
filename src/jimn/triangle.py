from jimn.point import *
from jimn.segment import *

class triangle(segment):
    def __init__(self, p1 = point3d(), p2 = point3d(), p3 = point3d()):
        segment.__init__(self, p1, p2)
        self.p3 = p3
    def __str__(self):
        return segment.__str__(self)[:-1] + " ; {}]".format(str(self.p3))

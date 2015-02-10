from jimn.point import *
from jimn.segment import *

class triangle(segment):
    def __init__(self, p1 = point(), p2 = point(), p3 = point()):
        segment.__init__(self, p1, p2)
        self.sommets.append(p3)
    def __str__(self):
        return segment.__str__(self)[:-1] + " ; {}]".format(str(self.sommets[2]))

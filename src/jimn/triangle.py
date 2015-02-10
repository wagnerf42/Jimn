# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

from jimn.point import *
from jimn.segment import *

class triangle:
    def __init__(self, p1 = point(), p2 = point(), p3 = point()):
        self.sommets = [p1, p2, p3]
    def __str__(self):
        return  "[{} ; {} ; {}]".format(str(self.sommets[0]), str(self.sommets[1]), str(self.sommets[2]))

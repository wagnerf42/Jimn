# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.segment import *
from jimn.point import *

class triangle:
    def __init__(self, p1 = point(), p2 = point(), p3 = point()):
        self.endpoints = [p1, p2, p3]
    def __str__(self):
        return  "[{} ; {} ; {}]".format(str(self.endpoints[0]), str(self.endpoints[1]), str(self.endpoints[2]))

    def higher(self, h):
        for p in self.endpoints:
            if p.coordinates[2] < h:
                return False;
        return True;

    def lower(self, h):
        for p in self.endpoints:
            z = p.coordinates[2]
            if z > h:
                return False;
        return True;

    def separate(self, h):
        inf_equal = []
        sup = []
        for p in self.endpoints:
            z = p.coordinates[2]
            if z <= h:
                inf_equal.append(p);
            else:
                sup.append(p);
        if len(inf_equal) == 2:
            return inf_equal, sup;
        else:
            return sup, inf_equal;

    def intersect(self, h):
        if self.higher(h):
            p1 = self.endpoints[0];
            p2 = self.endpoints[1];
            p3 = self.endpoints[2];
            lseg = [];
            lseg.append(segment(p1, p2));
            lseg.append(segment(p1, p3));
            lseg.append(segment(p2, p3));
            return lseg;
        if self.lower(h):
            return [];

        two, one = self.separate(h);
        p1 = two[0];
        p2 = two[1];
        q = one[0];

        s1 = segment(p1, q);
        s2 = segment(p2, q);
        i1 = s1.intersect(h);
        i2 = s2.intersect(h);

        lseg = [];
        z1 = p1.coordinates[2];
        if z1 <= h:
            lseg.append(segment(i1, i2));
            lseg.append(segment(i1, q));
            lseg.append(segment(i2, q));
        else:
            lseg.append(segment(i1, i2));
            lseg.append(segment(i1, p1));
            lseg.append(segment(i2, p2));
            lseg.append(segment(p1, p2));
        return lseg;

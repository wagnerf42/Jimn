# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.point import point
# TODO: what precision should we take ?


class coordinates_hash:
    def __init__(self):
        self.hashes = [{}, {}, {}]  # TODO remove third one
        self.displaced_hashes = [{}, {}, {}]  # TODO remove third one

    def hash_point(self, p):
        new_coordinates = []
        for i, c in enumerate(p.get_coordinates()):
            key = coordinate_key(c)
            displaced_key = coordinate_key(c+0.000005)
            if key in self.hashes[i]:
                c = self.hashes[i][key]
            else:
                self.hashes[i][key] = c
                if displaced_key in self.displaced_hashes[i]:
                    c = self.displaced_hashes[i][displaced_key]
                else:
                    self.displaced_hashes[i][displaced_key] = c
            new_coordinates.append(c)

        new_point = point(new_coordinates)
        return new_point


def coordinate_key(c):
    key = float("{0:.5f}".format(c))  # TODO: check about -0 and +0
    return key

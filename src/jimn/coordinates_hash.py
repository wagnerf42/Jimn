# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4
from jimn.precision import coordinate_key, displaced_coordinate_key
from jimn.point import point


class coordinates_hash:
    def __init__(self):
        self.hashes = [{}, {}, {}]  # TODO remove third one

    def hash_point(self, p):
        new_coordinates = []
        for i, c in enumerate(p.get_coordinates()):
            key = coordinate_key(c)
            displaced_key = displaced_coordinate_key(c)
            if key in self.hashes[i]:
                c = self.hashes[i][key]
            else:
                if displaced_key in self.hashes[i]:
                    c = self.hashes[i][displaced_key]

            self.hashes[i][key] = c
            self.hashes[i][displaced_key] = c
            new_coordinates.append(c)

        new_point = point(new_coordinates)
        return new_point

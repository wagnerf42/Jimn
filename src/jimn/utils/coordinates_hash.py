from jimn.point import point
from jimn.utils.debug import is_module_debugged
from jimn.utils.precision import coordinate_key, displaced_coordinate_key, \
    precision


class coordinates_hash:
    def __init__(self, dimension, wanted_precision=precision):
        self.hashes = []
        self.precision = wanted_precision
        for d in range(dimension):
            self.hashes.append({})

    def hash_coordinate(self, index, c):
        key = coordinate_key(c, self.precision)
        displaced_key = displaced_coordinate_key(c)

        if key in self.hashes[index]:
            c = self.hashes[index][key]
        else:
            if displaced_key in self.hashes[index]:
                c = self.hashes[index][displaced_key]

        self.hashes[index][key] = c
        self.hashes[index][displaced_key] = c

        return c

    def hash_point(self, p):
        new_coordinates = []
        for i, c in enumerate(p.get_coordinates()):
            new_c = self.hash_coordinate(i, c)
            new_coordinates.append(new_c)
            if __debug__:
                if is_module_debugged(__name__):
                    print("coordinate {} hashes to {}".format(c, new_c))

        new_point = point(new_coordinates)
        return new_point

rounder2d = coordinates_hash(2)
rounder_lines = coordinates_hash(2, precision-2)

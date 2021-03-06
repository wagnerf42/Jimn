"""
bounding boxes are rectangular boxes delimiting a set of items.
they are usually used in display to compute image sizes.
"""
from jimn.utils.precision import is_almost


class BoundingBox:
    """
    enclosing rectangles.
    """
    def __init__(self, min_coordinates, max_coordinates):
        self.min_coordinates = list(min_coordinates)
        self.max_coordinates = list(max_coordinates)

    def copy(self):
        """
        return deepcopy of given box.
        """
        return BoundingBox(
            list(self.min_coordinates), list(self.max_coordinates))

    @classmethod
    def empty_box(cls, dimension):
        """
        return an empty box in space of given dimension
        """
        min_coordinates = []
        max_coordinates = []
        for _ in range(dimension):
            min_coordinates.append(float('+inf'))
            max_coordinates.append(float('-inf'))
        return cls(min_coordinates, max_coordinates)

    def add_point(self, added_point):
        """
        register a point inside the box.
        update box limits if needed.
        """
        for i, added_coordinate in enumerate(added_point.coordinates):
            if added_coordinate < self.min_coordinates[i]:
                self.min_coordinates[i] = added_coordinate
            if added_coordinate > self.max_coordinates[i]:
                self.max_coordinates[i] = added_coordinate

    def contains(self, other):
        """
        return True if other box is inside ourselves (or equal).
        """
        for min1, min2 in zip(self.min_coordinates, other.min_coordinates):
            if min2 < min1:
                return False

        for max1, max2 in zip(self.max_coordinates, other.max_coordinates):
            if max2 > max1:
                return False

        return True

    def almost_contains_point(self, point):
        """
        return true if point is almost inside box.
        """
        for i, coordinate in enumerate(point.coordinates):
            if coordinate < self.min_coordinates[i] and \
                    (not is_almost(coordinate, self.min_coordinates[i])):
                return False
            if coordinate > self.max_coordinates[i] and \
                    (not is_almost(coordinate, self.max_coordinates[i])):
                return False
        return True

    def intersects(self, other):
        """
        returns if the two boxes intersect (even only on edge)
        """
        for i in range(len(self.min_coordinates)):
            small = self.min_coordinates[i]
            big = self.max_coordinates[i]
            other_small = other.min_coordinates[i]
            other_big = other.max_coordinates[i]
            if (other_small > big) or (other_big < small):
                return False
        return True

    def update(self, other):
        """
        update self box by taking constraints from other box into account
        """
        assert len(self.min_coordinates) == len(other.min_coordinates), \
            'merge different boxes'
        for i, coordinate in enumerate(other.min_coordinates):
            if self.min_coordinates[i] > coordinate:
                self.min_coordinates[i] = coordinate
        for i, coordinate in enumerate(other.max_coordinates):
            if self.max_coordinates[i] < coordinate:
                self.max_coordinates[i] = coordinate

    def limits(self, index):
        """
        returns array of limits for a given coordinate index
        """
        return (self.min_coordinates[index], self.max_coordinates[index])

    def inflate(self, distance):
        """
        get bigger box containing original box + any point outside
        original at distance less than given
        """
        self.min_coordinates = [c - distance for c in self.min_coordinates]
        self.max_coordinates = [c + distance for c in self.max_coordinates]

    def get_arrays(self):
        """
        returns arrays of limits
        """
        return (self.min_coordinates, self.max_coordinates)

    def get_bounding_box(self):
        """
        we can display boxes themselves.
        so just return ourselves if asked our dimenstions.
        """
        return self

    def save_svg_content(self, display, color):
        """
        svg code for displaying box.
        pre-requisite: 2d box
        """
        points = [
            Point(list(self.min_coordinates)),
            Point(list(self.max_coordinates))
        ]
        coordinates = [
            display.convert_coordinates(p.coordinates) for p in points
        ]
        stroke_width = display.stroke_width()
        for indices in (
                ((0, 0), (0, 1)),
                ((0, 1), (1, 1)),
                ((1, 1), (1, 0)),
                ((1, 0), (0, 0))
        ):
            display.write("<line x1=\"{}\" y1=\"{}\"\
                          x2=\"{}\" y2=\"{}\"".format(
                              coordinates[indices[0][0]][0],
                              coordinates[indices[0][1]][1],
                              coordinates[indices[1][0]][0],
                              coordinates[indices[1][1]][1]
                          ))
            display.write(" stroke-width=\"{}\" stroke=\"{}\"\
                          opacity=\"0.5\"/>\n".format(stroke_width, color))

    def __eq__(self, other):
        return (self.min_coordinates == other.min_coordinates) and\
            (self.max_coordinates == other.max_coordinates)

    def __str__(self):
        return('BoundingBox([{}], [{}])'.format(self.min_coordinates,
                                                self.max_coordinates))


from jimn.point import Point

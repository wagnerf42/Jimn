"""
factorizes some code for all objects containing sets of points.
"""


def nearest_point(self, point):
    """
    return nearest point in self from given point p.
    """
    # TODO: real nearest point (not just an endpoint)
    best_distance = float("+inf")
    for point2 in self.get_points():
        distance = point.distance_to(point2)
        if distance < best_distance:
            best_distance = distance
            best_point = point2

    return best_point


def nearest_points(self, other):
    """
    return nearest point between two points containers.
    """
    # TODO: real nearest point (not just an endpoint)
    best_distance = float("+inf")
    for point in self.get_points():
        for point2 in other.get_points():
            distance = point.distance_to(point2)
            if distance < best_distance:
                best_distance = distance
                best_points = (point, point2)
    return best_points

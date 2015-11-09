"""
factorizes some code for all objects containing sets of points
"""

def nearest_point(self, p):
    """
    returns nearest point in self from given point p.
    """
    #TODO: real nearest point
    best_distance = float("+inf")
    for p2 in self.get_points():
        d = p.distance_to(p2)
        if d < best_distance:
            best_distance = d
            best_point = p2

    return best_point

def nearest_points(self, other):
    """
    returns nearest point between two points containers
    """
    #TODO: real nearests points
    best_distance = float("+inf")
    for p in self.get_points():
        for p2 in other.get_points():
            d = p.distance_to(p2)
            if d < best_distance:
                best_distance = d
                best_points = (p, p2)
    return best_points

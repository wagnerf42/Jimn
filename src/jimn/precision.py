
precision = 7
limit = 10**-precision
squared_limit = limit * limit
precision_format = "{{0:.{}f}}".format(precision)


def is_almost(c1, c2):
    return abs(c2-c1) < limit


def coordinate_key(c):
    key = float(precision_format.format(c))
    return key


def displaced_coordinate_key(c):
    return coordinate_key(c+limit/2+limit/10)

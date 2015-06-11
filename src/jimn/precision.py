import sys

precision = 6
limit = 10**-precision
squared_limit = limit * limit
precision_format = "{{0:.{}f}}".format(precision)


def is_almost(c1, c2):
    return abs(c2-c1) < limit


def coordinate_key(c):
    key = precision_format.format(c)
    if float(key) == 0.0: #change any eventual -0 to +0
        key = precision_format.format(0.0)
    return key


def displaced_coordinate_key(c):
    return coordinate_key(c+limit)


def print_debug(c):
    print('c: {0:.10f}'.format(c))
    print(coordinate_key(c))
    print(displaced_coordinate_key(c))


def check_precision(c1, c2, msg):
    if is_almost(c1, c2):
        print('warning: potential precision problem : {0}'.format(msg), file=sys.stderr)
        print('c1: {0:.10f} c2: {1:.10f}'.format(c1, c2))
        print_debug(c1)
        print_debug(c2)

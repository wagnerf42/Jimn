"""
precision related routines.
this stuff is highly toxic so take care.
"""

PRECISION = 6
LIMIT = 10**-PRECISION
SEGMENT_LIMIT = LIMIT  # minimum length allowed for a segment
PRECISION_FORMAT = "{{0:.{}f}}".format(PRECISION)


def is_almost(coordinate1, coordinate2, limit=LIMIT):
    """
    are coordinates almost the same ?
    """
    return abs(coordinate2-coordinate1) < limit


def coordinate_key(coordinate, wanted_precision=PRECISION):
    """
    return string display of given coordinate with wanted precision.
    """
    if wanted_precision == PRECISION:
        used_format = PRECISION_FORMAT
    else:
        used_format = "{{0:.{}f}}".format(wanted_precision)

    key = used_format.format(coordinate)
    if float(key) == 0.0:  # change any eventual -0 to +0
        key = used_format.format(0.0)
    return key


def displaced_coordinate_key(coordinate, wanted_precision=PRECISION):
    """
    return string display of given coordinate
    displaced by half precision.
    """
    # TODO: just add 0.000000....01 to key ??
    wanted_limit = 10**-wanted_precision
    return coordinate_key(coordinate + wanted_limit, wanted_precision)


def print_debug(coordinate):
    """
    display coordinate with high precision and keys.
    """
    print("raw coordinate", coordinate)
    print('10 decimal places : {0:.10f}'.format(coordinate))
    print("key", coordinate_key(coordinate))
    print("displaced key", displaced_coordinate_key(coordinate))


def check_precision(coordinate1, coordinate2, debug_message):
    """
    assert given coordinates are different enough.
    """
    if is_almost(coordinate1, coordinate2):
        print('warning: potential precision problem : ', debug_message)
        print('c1: {0:.10f} c2: {1:.10f}'.format(coordinate1, coordinate2))
        print("diff:", abs(coordinate2-coordinate1), "limit:", LIMIT)
        print_debug(coordinate1)
        print_debug(coordinate2)
        raise Exception("prec")

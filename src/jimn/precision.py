

def is_almost(c1, c2):
    return abs(c2-c1) < 0.000001


def coordinate_key(c):
    key = float("{0:.5f}".format(c))  # TODO: check about -0 and +0
    return key

def all_three_elements(array):
    for i in range(len(array)):
        e1 = array[i]
        e2 = array[(i+1) % len(array)]
        e3 = array[(i+2) % len(array)]
        yield e1, e2, e3


def all_two_elements(array):
    for i in range(len(array)):
        e1 = array[i]
        e2 = array[(i+1) % len(array)]
        yield e1, e2


def all_pairs(array):
    even_elements = array[0:][::2]
    odd_elements = array[1:][::2]
    return zip(even_elements, odd_elements)

"iterators to ease code readability."


def all_two_elements(array):
    """
    iterate on all adjacent pairs in an array including last item, first item.
    """
    for index, element1 in enumerate(array):
        element2 = array[(index+1) % len(array)]
        yield element1, element2


def all_three_elements(array):
    """
    iterate on all adjacent triplets in an array.
    """
    for index, element1 in enumerate(array):
        element2 = array[(index+1) % len(array)]
        element3 = array[(index+2) % len(array)]
        yield element1, element2, element3


def all_combinations(array1, array2):
    """
    iterate on all combinations of one element of each array.
    """
    for i in array1:
        for j in array2:
            yield i, j

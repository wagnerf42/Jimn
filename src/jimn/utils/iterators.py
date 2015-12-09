"iterators to ease code readability"


def all_two_elements(array):
    for i in range(len(array)):
        e1 = array[i]
        e2 = array[(i+1) % len(array)]
        yield e1, e2

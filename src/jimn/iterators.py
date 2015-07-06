def all_three_elements(array):
    for i in range(len(array)):
        e1 = array[i]
        e2 = array[(i+1)%len(array)]
        e3 = array[(i+2)%len(array)]
        yield e1, e2, e3

def all_two_elements(array):
    for i in range(len(array)):
        e1 = array[i]
        e2 = array[(i+1)%len(array)]
        yield e1, e2

from math import sqrt
from jimn.precision import is_almost

def solve_quadratic_equation(a, b, c):
    """ solves a*x*x + b*y +c = 0
    careful : we do some rounding here:
    when delta is close from 0 we round it towards 0
    do not use if you do not understand what it does"""
    delta = b * b - 4 * a * c
    if is_almost(delta, 0):
        return [-b/(2*a)]
    else:
        if delta < 0:
            return []
        else:
            return [(-b-sqrt(delta))/(2*a), (-b+sqrt(delta))/(2*a)]


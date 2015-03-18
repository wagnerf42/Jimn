from jimn.segment import segment 
import operator
from math import atan2

def hash_points(lseg):
    dico = {}
    for seg in lseg:
        endpoints = seg.get_endpoints()
        for point_index, point in enumerate(endpoints):
            neighbor_index = (point_index + 1) % 2
            neighbor = endpoints[neighbor_index]

            if point in dico:
                dico[point].append(neighbor)
            else:
                dico[point] = [neighbor]
    return dico

# angle for 2d points, relative to horizontal line
def angle(p1, p2):
    x1 = p1.get_x()
    x2 = p2.get_x()
    y1 = p1.get_y()
    y2 = p2.get_y()

    return atan2(y2 - y1, x2 - x1)

def sort_dico(dico):
    for point, neighbors in dico.items():
        sorted_neighbors = sorted(neighbors, key=lambda neighbor: angle(point, neighbor))
        dico[point] = sorted_neighbors

def left_high(seg):
    endpoints = seg.get_endpoints()
    sorted_endpoints = sorted(endpoints, key = lambda endpoint: endpoint.get_x())
    return(sorted_endpoints[0].get_x(), sorted_endpoints[1].get_y())

def sort_points(seg):
    endpoints = seg.get_endpoints()
    sorted_endpoints = sorted(endpoints, key = lambda endpoint: endpoint.get_x())
    return sorted_endpoints


def sort_lseg(lseg):
    return sorted(lseg, key=lambda seg: left_high(seg))

# en cours
def build_poly(beg_seg, dico):
    poly = []
    sorted_points = sort_points(beg_seg)
    beg_point = sorted_points[0]
    prec_point = beg_point
    cour_point = sorted_points[1]
    while cour_point != beg_point:
        poly.append(cour_point)
        lneighbors = dico[cour_point]
        length = len(lneighbors)
        index = lneighbors.index(prec_point)
        next_index = (index+1)%length
        next_point = lneighbors[next_index]
        prec_point = cour_point
        cour_point = next_point

# en cours
def build_lpoly(sorted_lseg, dico):
    marked = {}
    lpoly = []
    for seg in sorted_lseg:
        if not(seg in marked):
            lpoly.append(build_poly(seg, dico))
            marked[seg] = True
    return lpoly

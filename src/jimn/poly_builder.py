from jimn.segment import segment
from math import atan2


def hash_points(segments):
    segments_by_points = {}
    for s in segments:
        endpoints, reversed_endpoints = s.get_endpoints(), reversed(s.get_endpoints())
        for (p1, p2) in (endpoints, reversed_endpoints):
            if p1 in segments_by_points:
                segments_by_points[p1].append(p2)
            else:
                segments_by_points[p1] = [p2]
    return segments_by_points


# angle for 2d points, relative to horizontal line
def angle(*points):
    (x1, y1), (x2, y2) = [p.get_coordinates() for p in points]
    return atan2(y2 - y1, x2 - x1)


def sort_segments_of_points(segments_by_points):
    for point, neighbors in segments_by_points.items():
        sorted_neighbors = sorted(neighbors, key=lambda neighbor: angle(point, neighbor))
        segments_by_points[point] = sorted_neighbors


def sort_points(seg):
    endpoints = seg.get_endpoints()
    sorted_endpoints = sorted(endpoints, key=lambda endpoint: endpoint.get_x())
    return sorted_endpoints


def sort_lseg(segments):
    return sorted(segments, key=lambda s: s.smallest_point())


def build_poly(beg_seg, dico, marked):
    poly = []
    sorted_points = sort_points(beg_seg)
    beg_point = sorted_points[0]
    poly.append(beg_point)
    prec_point = beg_point
    cour_point = sorted_points[1]
    marked[segment(prec_point, cour_point)] = True
    while cour_point != beg_point:
        poly.append(cour_point)
        lneighbors = dico[cour_point]
        length = len(lneighbors)
        index = lneighbors.index(prec_point)
        next_index = (index+1) % length
        next_point = lneighbors[next_index]
        prec_point = cour_point
        cour_point = next_point
        marked[segment(prec_point, cour_point)] = True
    return poly


def build_lpoly(sorted_lseg, sorted_dico):
    marked = {}
    lpoly = []
    for seg in sorted_lseg:
        if not(seg in marked):
            lpoly.append(build_poly(seg, sorted_dico, marked))
    return lpoly
